open Core_kernel.Std
open Lwt
open Lwt_react
open LTerm_geom
open LTerm_text
open LTerm_key

(** User input loop *)
let rec loop ui coord =
  LTerm_ui.wait ui >>= function
  | LTerm_event.Key{ code = Up } ->
    coord := { !coord with row = !coord.row - 1 };
    LTerm_ui.draw ui;
    loop ui coord
  | LTerm_event.Key{ code = Down } ->
    coord := { !coord with row = !coord.row + 1 };
    LTerm_ui.draw ui;
    loop ui coord
  | LTerm_event.Key{ code = Left } ->
    coord := { !coord with col = !coord.col - 1 };
    LTerm_ui.draw ui;
    loop ui coord
  | LTerm_event.Key{ code = Right } ->
    coord := { !coord with col = !coord.col + 1 };
    LTerm_ui.draw ui;
    loop ui coord
  | LTerm_event.Key{ code = Escape } ->
    return ()
  | ev ->
    LTerm_ui.draw ui;
    loop ui coord

(** Run this at every interval. Resets the 'move me' text'. We don't
    care about the event callback. *)
let call_on_timer ui coord =
  ignore (Lwt_engine.on_timer 1.05 true (fun _ ->
      coord := { row = 0; col = 0};
      LTerm_ui.draw ui))

let go ui coord =
  call_on_timer ui coord;
  loop ui coord

let draw_bg ctxt tiles =
  Background.from_tile_list tiles ctxt
  |> Background.render

let draw_gary ctxt =
  let open Sprites in
  let open Qsprite in
  let gary = Sprites.gary ~offsetx:1 ~offsety:1 ctxt in
  Qsprite.move gary 8 8;
  Qsprite.render gary

let draw ui matrix tiles =
  let size = LTerm_ui.size ui in
  let ctxt = LTerm_draw.context matrix size in
  Format.printf "Size: %s\n" @@ string_of_size size;
  LTerm_draw.clear ctxt;
  draw_bg ctxt tiles;
  draw_gary ctxt

let main tiles =
  Lazy.force LTerm.stdout
  >>= fun term ->
  (* Coordinates of the message. *)
  let coord = ref { row = 0; col = 0 } in
  (*let gary_coord = ref { row = 0; col = 0 } in*)
  LTerm_ui.create term (fun ui matrix -> draw ui matrix tiles)
  >>= fun ui ->
  Lwt.finalize (fun () -> go ui coord) (fun () -> LTerm_ui.quit ui)

let run_lwt tiles =
  try Lwt_main.run (main tiles)
  with
  | LTerm_draw.Out_of_bounds ->
    failwith "Rendering is ON: You need to zoom out on your terminal!\n"
  | _ -> ();
