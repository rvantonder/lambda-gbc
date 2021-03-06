open Pixel
open LTerm_draw
open Core_kernel.Std
open Color
open Logging

type t = Pixel.t list list

(** Assume tiles 256x256 *)
let from_tile_list ?(offset_y=0) ?(offset_x=0) tiles ctxt =
  let open LTerm_style in
  let open Lwt in
  log_render @@ sprintf "Background from %d tiles. Shift Offset X: %d Y: %d"
      (List.length tiles) offset_x offset_y;

  (*let offset_y = 0x20 in*)
  let screen_160_144 = List.slice tiles offset_y (144+offset_y) |>
                       List.map ~f:(fun l -> List.slice l offset_x (160+offset_x)) in
  List.mapi screen_160_144 ~f:(fun i rows ->
      List.mapi rows ~f:(fun j tile ->
          let tile = match tile with
            | 0,0,0 -> 54,54,54
            | t -> t in
          let color = lterm_color_of_tuple tile in
          let style = {none with background = Some color} in
          Pixel.create_with_lterm_style ~posx:i ~posy:j ~style ctxt))

(** render a background on screen GBC bounds is 160 x 144. Assume this was set,
    with correct offsets, in from_tile_list *)
let render (background : t) =
  log_render "Iterating over background, calling pixel.render";
  List.iteri background ~f:(fun j row ->
      List.iteri row ~f:(fun i pixel ->
          Pixel.render pixel))

let render_line (background : t) i =
  let open Option in
  log_render @@ sprintf "Rendering line %d" i;
  (List.nth background i >>= fun row ->
   List.iteri row ~f:(fun i pixel ->
       Pixel.render pixel) |> return) |> ignore
