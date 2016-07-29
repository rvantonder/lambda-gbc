open Pixel
open LTerm_draw

(** 8 x 8 *)
(** Rectangles don't have an associated style, so we need
to add that to context *)
type t = Pixel.t array array

val render : t -> unit

val move : t -> int -> int -> unit

val from_file : int -> int -> string -> context -> t
