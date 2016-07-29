open LTerm_draw
open Core_kernel.Std
open LTerm_style
open LTerm_geom
open Pixel
open Color
open Format
(** Sprites are 8x8 *)

(** Standard sprite size *)
type t = Pixel.t array array

(** Returns a blank sprite. We use [ctxt] to create a sub ctxt
    containing the sprite *)
let create ctxt dimx dimy : t =
  let map =
    Array.make_matrix ~dimx ~dimy ctxt in
  Array.mapi map ~f:(fun i row ->
      Array.mapi row ~f:(fun j pixel ->
          Pixel.create ~posx:i ~posy:j ctxt))

let create_8_8 ctxt =
  create ctxt 8 8

let set_style (color : Color.t) : LTerm_style.t =
  let open LTerm_style in
  let color = lterm_color_of_color color in
  let style = {none with background = Some color} in
  style

(** Populates a sprite from file. TODO use Pixel.update color *)
let from_file offsetx offsety filename ctxt : t =
  let map : t = create_8_8 ctxt in
  let lines =
    In_channel.read_lines filename in
  List.foldi ~init:() lines ~f:(fun i _ line ->
      String.foldi ~init:() line ~f:(fun j _ char ->
          let color =
            match char with
            | 'w' -> White
            | 'b' -> Black
            | 'r' -> Red
            | 'f' -> Flesh
            | c -> failwith (sprintf "Invalid color %c" c) in
          let create = Pixel.create_with_offset ~posx:i ~posy:j
              ~offsetx ~offsety ~color ctxt in
          map.(i).(j) <- create
        ));
  map

(** Render a sprite. To to do so, render each pixel. *)
let render (sprite : t) =
  Array.iteri sprite ~f:(fun j row ->
      Array.iteri row ~f:(fun i pixel ->
          Pixel.render pixel))

(** Move each pixel of the sprite *)
let move sprite x' y' : unit =
  Array.iter sprite ~f:(fun row ->
      Array.iter row ~f:(fun pixel ->
          Pixel.move pixel x' y'))
