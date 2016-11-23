open Core_kernel.Std
open Bap.Std
open Util
open Util.Util_word

open Logging

let write_word addr w ctxt =
  Stmt.eval [Util_bil.store_ addr w] ctxt |> Option.some

let to_string = function
  | 0 -> "v-blank"
  | 1 -> "lcd"
  | 2 -> "timer"
  | 3 -> "joypad"
  | _ -> failwith "Unknown interrupt"

let request ctxt i =
  let open Option in
  let open Util in
  (*log_interrupt @@ sprintf "New interrupt request %d:%s" i (to_string i);*)
  ctxt#mem_at_addr (w16 0xFF0F) >>= fun req ->
  let rq = set_bit req i in
  write_word (w16 0xFF0F) rq ctxt
