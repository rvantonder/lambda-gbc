open LTerm_style

type t = White | Flesh | Red | Black

let lterm_color_of_tuple (x,y,z) = rgb x y z

let lterm_color_of_color = function
  | White -> rgb 255 255 255
  | Flesh -> rgb 255 192 128
  | Red -> rgb 255 0 0
  | Black -> rgb 0 0 0

let style_of_color (color : t) : LTerm_style.t =
  let open LTerm_style in
  let color' = lterm_color_of_color color in
  let style = {none with background = Some color'} in
  style
