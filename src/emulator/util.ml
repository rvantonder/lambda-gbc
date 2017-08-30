open Unix
open Lazy
open Core_kernel.Std
open Bap.Std
open Format
open Gbc_segment
open Options

module Env = Z80_env

module Bap_word = Word
module Bap_bil = Bil

module Util_word : sig
  val w8  : int -> word
  val w16 : int -> word
end = struct
  let w ~width = Word.of_int ~width
  let w8  = w ~width:8
  let w16 = w ~width:16
end

open Util_word

module Util_bil : sig
  val i8  : int -> exp
  val i16 : int -> exp
  val true_  : exp
  val false_ : exp
  val store_ : addr:addr -> word -> stmt
end = struct
  let i = Bil.int
  let i8 x  = i (w8 x)
  let i16 x = i (w16 x)
  let false_ = i Word.b0
  let true_  = i Word.b1
  let store_ ~addr v =
    let open Bil in
    Env.mem :=
      Bil.store
        ~mem:(Bil.var Env.mem)
        ~addr:(i addr) (i v) LittleEndian `r8
end


let test_bit v bit_pos =
  let mask = Bap_word.(w8 1 lsl w8 bit_pos) in
  Bap_word.(mask land v) = Bap_word.(w8 1 lsl w8 bit_pos)

let set_bit v bit_pos =
  let mask = Bap_word.(w8 1 lsl w8 bit_pos) in
  Bap_word.(mask lor v)

let reset_bit v bit_pos =
  let mask = Bap_word.(w8 1 lsl w8 bit_pos) in
  Bap_word.(mask land (lnot v))

let time tag f options =
  let t = Unix.gettimeofday () in
  let lazy result = f in
  if options.v then
    Format.printf
      "[%s] Execution time: %f seconds\n%!"
      tag
      (Unix.gettimeofday () -. t);
  result

let dump_storage storage s e =
  List.iter (List.range s e) ~f:(fun addr_int ->
      let addr = Addr.of_int ~width:16 addr_int in
      if addr_int % 32 = 0 then printf "\n";
      match storage#load addr with
      | Some word ->
        printf "%02x " @@ (Bap_word.to_int word |> ok_exn)
      | None -> printf "00 ");
  printf "@."

let dump_segment storage (seg : Gbc_segment.t) =
  dump_storage storage seg.pos (seg.pos+seg.size)
