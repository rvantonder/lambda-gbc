open Sprite
open Qsprite

(** Sprites are 4 quadrants (sprites) of 8x8 *)

let gary ?(offsetx = 0) ?(offsety = 0) ctxt: Qsprite.t =
  let tl = Sprite.from_file offsetx offsety "sprites/gary-tl.sprite" ctxt in
  let tr = Sprite.from_file offsetx offsety "sprites/gary-tr.sprite" ctxt in
  let bl = Sprite.from_file offsetx offsety "sprites/gary-bl.sprite" ctxt in
  let br = Sprite.from_file offsetx offsety "sprites/gary-br.sprite" ctxt in
  {tl; tr; bl; br}
