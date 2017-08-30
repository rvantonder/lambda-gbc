open Unsigned
open Format

type t =
  { stmt : Z80_stmt.t     (* the disassembled statement *)
  ; position : int        (* absolute position in image *)
  ; bytes : uint8 list    (* bytes corresponding to statement *)
  ; cycles : int          (* number of clock cycles this statement takes *)
  }

val empty : unit -> t

val pp : formatter -> t -> unit

val to_string : t -> string

val pps : unit -> t -> string
