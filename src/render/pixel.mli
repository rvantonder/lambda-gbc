open LTerm_style
open LTerm_draw
open LTerm_geom
open Color

(** Needs rethinking: we use rect for pos, and x and y in pixel is its dimensions??*)

(** A pixel has a context (bounded box for drawing), a rectangle and
    style (e.g. color *)
type t

val create : ?posx:int -> ?posy:int -> ?color:Color.t -> context -> t
val create_with_offset : ?offsetx:int -> ?offsety:int -> ?posx:int -> ?posy:int -> ?color:Color.t -> context -> t

(*val move : t -> (int -> int -> (int * int)) -> *)

val render : t -> unit

(** Big question: if I update elements part of the sub context, are
    they updated, or do i have to create a new context? *)

val move : t -> int -> int -> unit
