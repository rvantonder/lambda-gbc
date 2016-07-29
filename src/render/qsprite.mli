open LTerm_draw

type t = {tl : Sprite.t;
          bl : Sprite.t;
          tr : Sprite.t;
          br: Sprite.t}

val render : t -> unit

(* Move x y *pixels* *)
val move : t -> int -> int -> unit
