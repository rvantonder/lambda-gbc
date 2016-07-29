open Sprite

(** sprites are composed of 4 8x8 sprites typically *)
type t = {tl : Sprite.t;
          bl : Sprite.t;
          tr : Sprite.t;
          br: Sprite.t}

let render qsprite =
  Sprite.render qsprite.tl;
  Sprite.render qsprite.tr;
  Sprite.render qsprite.bl;
  Sprite.render qsprite.br

let move qsprite x y =
  Sprite.move qsprite.tl x y;
  Sprite.move qsprite.tr x (y+8);
  Sprite.move qsprite.bl (x+8) y;
  Sprite.move qsprite.br (x+8) (y+8);
