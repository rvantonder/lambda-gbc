open Core_kernel.Std
open Format
open Unsigned
open Bap.Std

type t =
  { stmt : Z80_stmt.t
  ; position : int
  ; bytes : uint8 list
  ; cycles : int
  }

let empty () =
  { stmt = (`Undef,[])
  ; position = 0
  ; bytes = []
  ; cycles = 0
  }

let pp ppf hunk =
  List.map hunk.bytes ~f:UInt8.to_int
  |> List.map ~f:(fun i -> sprintf "%02x" i)
  |> String.concat
  |> fun bytes ->
  Format.sprintf
    "0x%08x %10s %s"
    hunk.position
    bytes
    (Z80_stmt.to_string hunk.stmt)
  |> Format.fprintf ppf "%s"

let to_string hunk = Format.asprintf "%a" pp hunk

let pps () hunk = to_string hunk
