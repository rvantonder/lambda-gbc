open LTerm_style
open LTerm_draw
open LTerm_geom
open Color

(** A pixel has a context (bounded box for drawing), a rectangle and style (e.g.
    color *)
type t

val create : ?posx:int -> ?posy:int -> ?color:Color.t -> context -> t

val create_with_lterm_style : ?posx:int -> ?posy:int ->
  style:LTerm_style.t -> context -> t

val create_with_offset : ?offsetx:int -> ?offsety:int ->
  ?posx:int -> ?posy:int -> ?color:Color.t -> context -> t

val render : t -> unit

val move : t -> int -> int -> unit
