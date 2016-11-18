open Core_kernel.Std
open Bap.Std

let log_gpu s =
  let section = Lwt_log.Section.make "gpu" in
  Lwt_log.ign_debug_f ~section "%s" s

let scanline_counter = ref 456

(** FF44 - LY - LCDC Y-Coordinate (R)
*)

let test_fake_read_of_ctxt' ctxt =
  let ly = Addr.of_int ~width:16 0xFF44 in
  match ctxt#mem_at_addr ly with
  | Some currentline -> log_gpu @@
    sprintf "DEBUG update: %a" Word.pps currentline
  | None -> failwith "Bad try"

  let w = Word.of_int ~width:8

(* TODO: fixup typing on debugger versus normal itnerpreter *)
let update interp (ctxt: Z80_interpreter_debugger.context) cycles =
  let open Option in
  (* TODO set LCD status *)
  let lcd_enabled = true in

  let write_word addr w ctxt =
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
      test_fake_read_of_ctxt' ctxt';
      Some ctxt'
    | _ -> None in

  (if lcd_enabled then
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
          write_word ly Word.(currentline + w 1) ctxt (* inc scanline*)
          >>= fun ctxt' ->
          log_gpu "Have fresh ctxt'";
          if currentline = w 144 then
            (log_gpu "request vblank interrupt";
             return ctxt') (* todo request interrupt *)
          else if currentline > w 153 then
            (log_gpu @@ sprintf
               "currentline %a past scanline 153, resetting 0"
               Word.pps currentline;
             write_word ly (w 0) ctxt)
          else if currentline < w 144 then
            (log_gpu @@ sprintf "drawing scanline %a" Word.pps currentline;
             test_fake_read_of_ctxt' ctxt';
             return ctxt') (* todo draw scanline*)
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
