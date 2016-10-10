open Pixel
open LTerm_draw

type t

val render : t -> unit

val from_tile_list : ?offset_y : int -> ?offset_x : int -> (int * int * int) list list -> context -> t
