open Pixel
open LTerm_draw

type t

val render : t -> unit

val from_tile_list : (int * int * int) list list -> context -> t
