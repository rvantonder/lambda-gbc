(** Linear sweep disassemble the image *)

(** Threading state through with CPS is just beautiful, if you choose
    the state variables correctly *)

(** Think ICFP for monads *)

open Core_kernel.Std
open Format
open Unsigned

module Hunk : sig
  type t =
    { stmt : Z80_stmt.t;     (* the disassembled statement *)
      position : int;        (* absolute position in image *)
      bytes : uint8 list;    (* bytes corresponding to statement *)
      cycles : int           (* number of clock cycles this statement takes *)
    }

  val empty : unit -> t

  val pp : formatter -> t -> unit
  val to_string : t -> string
  val pps : unit -> t -> string
end

type hunk = Hunk.t

(** Linear disassemble from the [start] position, up to [size] bytes.
    If size is not supplied, disassemble all (TODO) *)
val linear : Z80_image.t -> start:int -> size:int -> hunk list

val pp : formatter -> hunk list -> unit

val to_string : hunk list -> string

val pps : unit -> hunk list -> string
