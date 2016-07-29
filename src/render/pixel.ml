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

(** Dimension is used for screen. For terminal, we need x =1 and y =2*)
type dim = {x : int ; y : int}

let dim = {x = 1; y = 2}

let create ?(posx = 0) ?(posy = 0) ?(color = White) screen =
  let rect =  {row1 = dim.x*posx; row2 = dim.x*posx+dim.x;
               col1 = dim.y*posy; col2 = dim.y*posy+dim.y} in
  let style = style_of_color color in
  {posx; posy; rect; style; screen}

let create_with_offset ?(offsetx = 0) ?(offsety = 0)
    ?(posx = 0) ?(posy = 0) ?(color = White) screen =
  let rect = {row1 = (posx+offsetx)*dim.x;
              row2 = (posx+offsetx)*dim.x+dim.x;
              col1 = (posy+offsety)*dim.y;
              col2 = (posy+offsety)*dim.y+dim.y} in
  let style = style_of_color color in
  {posx; posy; rect; style; screen}

let render pixel =
  let ctxt = LTerm_draw.sub pixel.screen pixel.rect in
  LTerm_draw.fill_style ctxt pixel.style

let move pixel offsetx offsety =
  let posx = pixel.posx and posy = pixel.posy in
  pixel.rect <- {row1 = (posx+offsetx)*dim.x;
                 row2 = (posx+offsetx)*dim.x+dim.x;
                 col1 = (posy+offsety)*dim.y;
                 col2 = (posy+offsety)*dim.y+dim.y};
