open Core_kernel.Std
open LTerm
open Format
open Unsigned
open Bap.Std

open Z80_image
open Z80_disasm

open Hunk

let verbose = false
(** Print the 4 bytes that we are disassembling *)
let debug = false

let print pos bytes (stmt : Z80_stmt.t) =
  let open LTerm_text in
  let open LTerm_style in
  List.map bytes ~f:UInt8.to_int
  |> List.map ~f:(fun i -> sprintf "%02x" i)
  |> String.concat
  |> fun bytes ->
  let green = "\x1b[42m" in
  let restore = "\x1b[40m" in
  if verbose then
    Format.printf
      "0x%08x %10s %s%s%s\n"
      pos
      bytes
      green
      (Z80_stmt.to_string stmt)
      restore

(** Print the 4 bytes we are currently disassembling *)
let print_bytes bytes =
  if debug then begin
    List.iter bytes ~f:(fun i -> printf " %02x " (UInt8.to_int i));
    printf "@."
  end

(** Continue extracts the first byte at the position and then calls
    [dis] on it. [dis] invokes our function f which processes the
    result, and then calls continue again *)
let rec decode_cc
    image
    stop
    position
    (hunk_accumulator : Hunk.t list)
  : Hunk.t list =
  match position > (stop - 4) with
  | true -> hunk_accumulator
  | false ->
    let bytes =
      get_bytes image ~position ~size:4
      |> Array.to_list in

    print_bytes bytes;

    (* I made a doot. There's no need to pass the entire f
       continuation function to decode, just so we can populate the
       last two arguments. Break it up and extract those first. Since
       I don't want to break my convention of space-separated args
       that would fill up the continuation function, I just write an
       'extract' function that will return for me the tuple *)
    let extract x y z = x, y, z in
    let stmt, advance, cycles = Z80_decoder.decode bytes position extract in
    let next_position = position + advance in

    (* The bytes we receive is always 4. but we only want 'advance' amount. so
       extract that amount and assign that to bytes *)
    let bytes = List.take bytes advance in
    print position bytes stmt;
    let hunk = { stmt; position; bytes; cycles } in
    decode_cc image stop next_position (hunk::hunk_accumulator)

(** Initiate the continuation *)
let linear (image : Z80_image.t) ~start ~size : Hunk.t list =
  decode_cc image (start+size) start [] |> List.rev

let pp ppf hunks =
  let res =  List.map hunks ~f:Hunk.to_string in
  Format.fprintf ppf "%s@." @@ String.concat ~sep:"\n" res

let to_string stmts = Format.asprintf "%a" pp stmts

let pps () stmts = to_string stmts
