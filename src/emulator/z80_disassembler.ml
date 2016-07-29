open Core_kernel.Std
open LTerm
open Format
open Unsigned
open Bap.Std

open Z80_image
open Z80_disasm

module Hunk = struct
  type t =
    { stmt : Z80_stmt.t;
      position : int;
      bytes : uint8 list;
      cycles : int}

  let empty () =
    { stmt = (`Undef,[]);
      position = 0;
      bytes = [];
      cycles = 0}

  let pp ppf hunk =
    let res =
      let bytes =
        List.map hunk.bytes ~f:UInt8.to_int
        |> List.map ~f:(fun i -> sprintf "%02x" i)
        |> String.concat in
      Format.sprintf "0x%08x %10s %s"
        hunk.position bytes (Z80_stmt.to_string hunk.stmt) in
    Format.fprintf ppf "%s" res

  let to_string hunk = Format.asprintf "%a" pp hunk

  let pps () hunk = to_string hunk

end

type hunk = Hunk.t
open Hunk

let verbose = false
(** Print the 4 bytes that we are disassembling *)
let debug = false

let print_radare pos bytes (stmt : Z80_stmt.t) =
  let open LTerm_text in
  let open LTerm_style in
  let bytes =
    List.map bytes ~f:UInt8.to_int
    |> List.map ~f:(fun i -> sprintf "%02x" i)
    |> String.concat in
  let green = "\x1b[42m" in
  let restore = "\x1b[40m" in
  if verbose then
    Format.printf "0x%08x %10s %s%s%s\n"
      pos bytes green (Z80_stmt.to_string stmt) restore

(** Deprecated, no need to have anything to do with lifting in the
    disassembler *)
let print_lifted (stmts : Bil.t) =
  let green = "\x1b[42m" in
  let restore = "\x1b[40m" in
  if verbose then
    List.iter stmts ~f:(fun stmt ->
        Format.printf "%22s%s%s%s\n" " "
          green (Bap.Std.Stmt.to_string stmt) restore)

let print_pos pos =
  if verbose then Format.printf "0x%08xp\n" pos

let print_stmt stmt =
  if verbose then Format.printf "%a\n" Z80_stmt.pp stmt

(** Print the 4 bytes we are currently disassembling *)
let print_bytes bytes =
  if debug then
    (List.iter bytes ~f:(fun i -> printf " %02x " (UInt8.to_int i));
     printf "\n")

(** Continue extracts the first byte at the position and then calls
    [dis] on it. [dis] invokes our function f which processes the
    result, and then calls continue again *)
let rec continue image stop f position (acc : hunk list) : hunk list =
  if position > (stop - 4) then acc else
    let bytes = get_bytes image ~position ~size:4 |> Array.to_list in
    print_bytes bytes;

    (* Partial continuation. Supplied with image, stop, and f. Needs
       next_position and acc *)
    let continue = continue image stop f in

    (* I made a doot. There's no need to pass the entire f
       continuation function to decode, just so we can populate the
       last two arguments. Break it up and extract those first. Since
       I don't want to break my convention of space-separated args
       that would fill up the continuation function, I just write an
       'extract' function that will return for me the tuple *)
    let extract x y z = x,y,z in
    let stmt,advance,cycles = Z80_decoder.decode bytes position extract in
    (* I will now call f on the accumulator and position, and also
       give you the continue function *)
    f acc position continue bytes image stmt advance cycles

(** f: where to disassemble next, and accumulator. next could
    be put in continue? *)
(** Process the output of dis, and update the position accordingly.
    Then continue. dis populates [dis] and [advance]. the rest is
    threaded state.*)
(** Later, image was added as an argument since we want to pass this
    to lift *)
let f (acc : hunk list) position continue (bytes : UInt8.t list) image
    dis advance cycles =
  let next_position = position + advance in

  match dis with
  (* can optionally silently continue if dis is `Undef, for linear.
     but i don't want this behaviour on interpreter *)
  (*| (`Undef,_) -> continue next_position acc*)
  | stmt ->
    (* the bytes we receive is always 4. but we only want 'advance'
       amount. so extract that amount and assign that to bytes *)
    let bytes = (List.take bytes advance) in
    print_radare position bytes stmt;
    let hunk = {stmt; position; bytes; cycles} in
    continue next_position (hunk::acc)

(** Initiate the continuation *)
let linear (image : Z80_image.t) ~start ~size : hunk list =
  continue image (start+size) f start [] |> List.rev

let pp ppf hunks =
  let res =  List.map hunks ~f:Hunk.to_string in
  Format.fprintf ppf "%s\n" @@ String.concat ~sep:"\n" res

let to_string stmts = Format.asprintf "%a" pp stmts

let pps () stmts = to_string stmts
