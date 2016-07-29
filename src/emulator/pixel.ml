open LTerm_style
open LTerm_draw
open LTerm_geom
open Color

(** A pixel has a context (rectangle) and style (e.g. color *)
type t = {screen: context;
          style : LTerm_style.t;
          mutable rect : rect;
          posx : int;
          posy : int}

(** Aspect ratio for screen. For terminal, we need x = 1 and y = 2, or
    y = 3, depending on the zoom*)
type aspect = {x : int ; y : int}

let aspect = {x = 1; y = 3}

let create ?(posx = 0) ?(posy = 0) ?(color = White) screen =
  let rect =  {row1 = aspect.x*posx; row2 = aspect.x*posx+aspect.x;
               col1 = aspect.y*posy; col2 = aspect.y*posy+aspect.y} in
  let style = style_of_color color in
  {posx; posy; rect; style; screen}

let create_with_lterm_style ?(posx = 0) ?(posy = 0) ~style screen =
  let rect =  {row1 = aspect.x*posx; row2 = aspect.x*posx+aspect.x;
               col1 = aspect.y*posy; col2 = aspect.y*posy+aspect.y} in
  {posx; posy; rect; style; screen}

let create_with_offset ?(offsetx = 0) ?(offsety = 0)
    ?(posx = 0) ?(posy = 0) ?(color = White) screen =
  let rect = {row1 = (posx+offsetx)*aspect.x;
              row2 = (posx+offsetx)*aspect.x+aspect.x;
              col1 = (posy+offsety)*aspect.y;
              col2 = (posy+offsety)*aspect.y+aspect.y} in
  let style = style_of_color color in
  {posx; posy; rect; style; screen}

let render pixel =
  let ctxt = LTerm_draw.sub pixel.screen pixel.rect in
  LTerm_draw.fill_style ctxt pixel.style

(* TODO, doesn't work yet *)
let move pixel offsetx offsety =
  let posx = pixel.posx and posy = pixel.posy in
  pixel.rect <- {row1 = (posx+offsetx)*aspect.x;
                 row2 = (posx+offsetx)*aspect.x+aspect.x;
                 col1 = (posy+offsety)*aspect.y;
                 col2 = (posy+offsety)*aspect.y+aspect.y};
