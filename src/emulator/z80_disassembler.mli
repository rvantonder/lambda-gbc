(** Linear sweep disassemble the image *)

(** Threading state through with CPS is just beautiful, if you choose
    the state variables correctly *)

(** Think ICFP for monads *)

open Core_kernel.Std
open Format

(** Linear disassemble from the [start] position, up to [size] bytes.
    If size is not supplied, disassemble all (TODO) *)
val linear : Z80_image.t -> start:int -> size:int -> Hunk.t list

val pp : formatter -> Hunk.t list -> unit

val to_string : Hunk.t list -> string

val pps : unit -> Hunk.t list -> string
