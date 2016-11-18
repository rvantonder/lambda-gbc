open Core_kernel.Std
open Bap.Std

let scanline_counter = ref 0

(** FF44 - LY - LCDC Y-Coordinate (R)
*)

let w = Word.of_int ~width:16

(* TODO: fixup typing on debugger versus normal itnerpreter *)
let update (ctxt: Z80_interpreter_debugger.context) cycles =
  let open Option in
  (* TODO set LCD status *)

  let ly = Addr.of_int ~width:16 0xFF44 in

  if !scanline_counter <= 0 then
    ctxt#mem_at_addr ly >>= fun currentline ->
    ctxt#write_word ly currentline >>= fun ctxt' -> (* inc scanline*)
    if currentline = w 144 then return ctxt' (* todo request interrupt *)
    else if currentline > w 153 then
      ctxt'#write_word ly (w 0)
    else if currentline < w 144 then return ctxt' (* todo draw scanline*)
    else return ctxt'
  else
    return ctxt
