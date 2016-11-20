open Core_kernel.Std
open Bap.Std
open Util

let log_gpu s =
  let section = Lwt_log.Section.make "gpu" in
  Lwt_log.ign_debug_f ~section "%s" s

let scanline_counter = ref 456

(** FF44 - LY - LCDC Y-Coordinate (R)
*)

(* TODO: initial lcd enabled is 3E. should i be servicing some
   interrupt first? *)
let is_lcd_enabled ctxt =
  let open Option in
  match ctxt#mem_at_addr (w16 0xFF40) with
  | Some lcd_byte ->
    log_gpu @@ sprintf "is_lcd_enabled testing addr %a. value is %a"
    Word.pps (w16 0xFF40) Word.pps lcd_byte;
      test_bit lcd_byte 7
  | None -> false

(* TODO remove interp *)
let write_word addr w (ctxt : Z80_interpreter_debugger.context) interp :
Z80_interpreter_debugger.context option =
  let open Option in
  let open Z80_cpu.CPU in
  let store_ addr v = Bil.store ~mem:(Bil.var Z80_cpu.CPU.mem)
      ~addr:(Bil.int addr) (Bil.int v) LittleEndian `r8 in
  let stmt = [Bil.(Z80_cpu.CPU.mem := store_ addr w)] in
  ctxt#lookup (Z80_env.mem) >>= fun result ->
  match Bil.Result.value result with
  | Bil.Mem storage ->
    let start = interp#eval stmt in
    (* TODO may need other interp so things dont get caught later *)
    let ctxt' = Monad.State.exec start ctxt in
    Some ctxt'
  | _ -> None

(*http://www.codeslinger.co.uk/pages/projects/gameboy/lcd.html*)
let set_lcd_status (ctxt : Z80_interpreter_debugger.context) interp :
  Z80_interpreter_debugger.context option =
  let open Option in
  ctxt#mem_at_addr (w16 0xFF41) >>= fun lcdstatus ->
  match is_lcd_enabled ctxt with
  | false ->
    (*set the mode to 1 during lcd disabled and reset scanline*)
    scanline_counter := 456;
    let ly = Addr.of_int ~width:16 0xFF44 in
    write_word ly (w8 0) ctxt interp >>= fun ctxt' ->
    let status' = Word.(lcdstatus land w8 252) in
    let status' = set_bit status' 0 in
    log_gpu "LCD disabled during set_lcd_status. \
             Reset scanline and set mode to 1";
    write_word (w16 0xFF41) status' ctxt' interp
  | true ->
    let ly = Addr.of_int ~width:16 0xFF44 in
    ctxt#mem_at_addr ly >>= fun currentline ->
    let current_mode = Word.(lcdstatus land w8 3) in
    let mode,status',reqint =
      if currentline >= w8 144 then
        let status' = set_bit lcdstatus 0 in
        let status' = reset_bit status' 1 in
        let reqint = test_bit status' 4 in
        log_gpu "LCD set mode to 1";
        1,status',reqint
      else
        let mode2bounds = 456-80 in
        let mode3bounds = mode2bounds-172 in
        if !scanline_counter >= mode2bounds then
          let status' = set_bit lcdstatus 1 in
          let status' = reset_bit status' 0 in
          let reqint = test_bit status' 5 in
          log_gpu "LCD set mode to 2";
          2,status',reqint
        else if !scanline_counter >= mode3bounds then
          let status' = set_bit lcdstatus 1 in
          let status' = set_bit status' 0 in
          log_gpu "LCD set mode to 3";
          3,status',false
        else
          let status' = reset_bit lcdstatus 1 in
          let status' = reset_bit status' 0 in
          let reqint = test_bit status' 3 in
          log_gpu "LCD set mode to 0";
          0,status',reqint
    in
    (*just entered a new mode so request interupt*)
    match reqint && (not (w8 mode = current_mode)) with
    | true ->
      log_gpu @@
      sprintf "Mode switch from %a to %a. Requesting lcd interrupt"
        Word.pps (w8 mode) Word.pps (current_mode);
      Interrupts.request interp ctxt 1 >>= fun ctxt ->
      write_word (w16 0xFF41) status' ctxt interp
    | false ->
      ctxt#mem_at_addr (w16 0xFF45) >>= fun coincidence_flag ->
      let status',ctxt' =
        if coincidence_flag = currentline then
          (log_gpu "Coincidence flag and current scanline are same";
           let status' = set_bit status' 2 in
           let ctxt' =
             if test_bit status' 6 then
               (log_gpu
                  "Coincidence interrupt enabled. Requesting lcd interrupt";
                Interrupts.request interp ctxt 1)
             else
               Some ctxt
           in
           status',ctxt')
        else
          reset_bit status' 2,Some ctxt
      in
      ctxt' >>= fun ctxt' ->
      write_word (w16 0xFF41) status' ctxt interp

(* TODO: fixup typing on debugger versus normal itnerpreter *)
(*http://imrannazar.com/GameBoy-Emulation-in-JavaScript:-GPU-Timings*)
let update interp (ctxt: Z80_interpreter_debugger.context) cycles =
  let open Option in

  log_gpu "Setting LCD status";
  (set_lcd_status ctxt interp >>= fun ctxt ->

   let lcd_enabled = is_lcd_enabled ctxt in (* TODO *)
   (*let lcd_enabled = true in*)

   log_gpu "Continuing with GPU update";
   log_gpu @@ sprintf "LCD enabled : %b" lcd_enabled;

   if lcd_enabled then
     (
       scanline_counter := !scanline_counter - cycles;
       log_gpu @@ sprintf "subtracting %d scanline counter: %d"
         cycles
         !scanline_counter;

       let ly = Addr.of_int ~width:16 0xFF44 in
       ctxt#mem_at_addr ly >>= fun currentline ->

       log_gpu @@ sprintf "Current scanline: %a" Word.pps currentline;

       if !scanline_counter <= 0 then
         (scanline_counter := 456;
          log_gpu "scanline counter set to 456";
          log_gpu "incrementing current scanline";
          write_word ly Word.(currentline + w8 1) ctxt interp
          >>= fun ctxt' ->
          log_gpu "Have fresh ctxt'";
          if currentline = w8 144 then
            (log_gpu @@
             sprintf "Scanline is 144. Requesting v-blank interrupt";
             Interrupts.request interp ctxt' 0)
          else if currentline > w8 153 then (* fake the v-blank period*)
            (log_gpu @@ sprintf
               "currentline %a past scanline 153, resetting 0"
               Word.pps currentline;
             write_word ly (w8 0) ctxt interp)
          else if currentline < w8 144 then
            (* TODO see if we can ignore this. Not a long term solution
               .consider that the vram banks may switch between rendering *)
            (log_gpu @@ sprintf "drawing scanline %a" Word.pps currentline;
             return ctxt')
          else
            (log_gpu "no gpu update, returning ctxt'";
             return ctxt')
         )
       else
         return ctxt)
   else
     return ctxt)
  |> function
  | Some ctxt -> ctxt
  | None -> failwith "Failed in GPU.ml"
