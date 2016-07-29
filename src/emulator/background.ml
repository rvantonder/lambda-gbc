open Pixel
open LTerm_draw
open Core_kernel.Std
open Color

type t = Pixel.t list list

(** Assume tiles 256x256 *)
let from_tile_list tiles ctxt =
  let open LTerm_style in
  List.mapi tiles ~f:(fun i rows ->
      List.mapi rows ~f:(fun j tile ->
          let tile = match tile with
            | 0,0,0 -> 54,54,54
            | t -> t in
          let color = lterm_color_of_tuple tile in
          let style = {none with background = Some color} in
          Pixel.create_with_lterm_style ~posx:i ~posy:j ~style ctxt))

(** render a background on screen *)
let render (background : t) =
  List.iteri background ~f:(fun j row ->
      List.iteri row ~f:(fun i pixel ->
          Pixel.render pixel))
