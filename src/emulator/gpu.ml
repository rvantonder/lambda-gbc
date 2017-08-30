open Core_kernel.Std
open Bap.Std
open Logging

open Util
open Util.Util_word

let scanline_counter = ref 456

(** FF44 - LY - LCDC Y-Coordinate (R) *)

(* TODO: initial lcd enabled is 3E. Should I be servicing some interrupt
   first? *)
let is_lcd_enabled ctxt =
  let open Option in
  match ctxt#mem_at_addr (w16 0xFF40) with
  | Some lcd_byte -> test_bit lcd_byte 7
  | None -> false

let write_word addr w ctxt =
  Stmt.eval [Util_bil.store_ addr w] ctxt |> Option.some

(*http://www.codeslinger.co.uk/pages/projects/gameboy/lcd.html*)
let set_lcd_status (ctxt : Z80_interpreter_debugger.context) :
  Z80_interpreter_debugger.context option =
  let open Option in
  ctxt#mem_at_addr (w16 0xFF41) >>= fun lcdstatus ->
  match is_lcd_enabled ctxt with
  | false ->
    (* set the mode to 1 during lcd disabled and reset scanline*)
    scanline_counter := 456;
    let ly = w16 0xFF44 in
    write_word ly (w8 0) ctxt >>= fun ctxt' ->
    let status' = Word.(lcdstatus land w8 252) in
    let status' = set_bit status' 0 in
    (* LCD disabled during set_lcd_status. Reset scanline and set mode to 1 *)
    write_word (w16 0xFF41) status' ctxt'
  | true ->
    let ly = w16 0xFF44 in
    ctxt#mem_at_addr ly >>= fun currentline ->
    let current_mode = Word.(lcdstatus land w8 3) in
    let mode, status', reqint =
      if currentline >= w8 144 then
        let status' = set_bit lcdstatus 0 in
        let status' = reset_bit status' 1 in
        let reqint = test_bit status' 4 in
        (*log_gpu "LCD set mode to 1";*)
        1, status', reqint
      else
        let mode2bounds = 456-80 in
        let mode3bounds = mode2bounds-172 in
        if !scanline_counter >= mode2bounds then
          let status' = set_bit lcdstatus 1 in
          let status' = reset_bit status' 0 in
          let reqint = test_bit status' 5 in
          (*log_gpu "LCD set mode to 2";*)
          2, status', reqint
        else if !scanline_counter >= mode3bounds then
          let status' = set_bit lcdstatus 1 in
          let status' = set_bit status' 0 in
          (*log_gpu "LCD set mode to 3";*)
          3, status', false
        else
          let status' = reset_bit lcdstatus 1 in
          let status' = reset_bit status' 0 in
          let reqint = test_bit status' 3 in
          (*log_gpu "LCD set mode to 0";*)
          0, status', reqint
    in
    (*just entered a new mode so request interupt*)
    match reqint && (not (w8 mode = current_mode)) with
    | true ->
      Interrupts.request ctxt 1 >>= fun ctxt ->
      write_word (w16 0xFF41) status' ctxt
    | false ->
      ctxt#mem_at_addr (w16 0xFF45) >>= fun coincidence_flag ->
      let status',ctxt' =
        match coincidence_flag = currentline with
        | true ->
          (* log_gpu "Coincidence flag and current scanline are same"; *)
          let status' = set_bit status' 2 in
          let ctxt' =
            if test_bit status' 6 then Interrupts.request ctxt 1
            else Some ctxt
          in
          status', ctxt'
        | flase -> reset_bit status' 2,Some ctxt
      in
      ctxt' >>= fun ctxt' ->
      write_word (w16 0xFF41) status' ctxt

(* TODO: fixup typing on debugger versus normal interpreter *)
(* http://imrannazar.com/GameBoy-Emulation-in-JavaScript:-GPU-Timings *)
let update (ctxt: Z80_interpreter_debugger.context) cycles =
  let open Option in
  begin
    set_lcd_status ctxt >>= fun ctxt ->

    let lcd_enabled = is_lcd_enabled ctxt in (* TODO *)
    (*let lcd_enabled = true in*)

    (*log_gpu "Continuing with GPU update";
      log_gpu @@ sprintf "LCD enabled : %b" lcd_enabled;*)

    match lcd_enabled with
    | false -> return ctxt
    | true ->
      scanline_counter := !scanline_counter - cycles;
      let ly = w16 0xFF44 in
      ctxt#mem_at_addr ly >>= fun currentline ->
      match !scanline_counter <= 0 with
      | false -> return ctxt
      | true ->
        (*scanline counter set to 456 *)
        scanline_counter := 456;
        (* incrementing current scanline *)
        write_word ly Word.(currentline + w8 1) ctxt
        (* Have fresh ctxt' *)
        >>= fun ctxt' ->
        (* Scanline is 144. Requesting v-blank interrupt *)
        if currentline = w8 144 then Interrupts.request ctxt' 0
        (* Fake the v-blank period *)
        (* currentline past scanline 153 *)
        else if currentline > w8 153 then write_word ly (w8 0) ctxt
        (* TODO see if we can ignore this. Not a long term solution consider
           that the vram banks may switch between rendering *)
        else if currentline < w8 144 then return ctxt'
        else return ctxt'
  end
  |> Option.value_exn ~message:"Failed in GPU.ml"
