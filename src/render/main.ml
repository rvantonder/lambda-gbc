(*#use "topfind";; (* for running in toplevel *)
  #require "core_kernel";;
  #require "lambda-term";;
  #require "lwt";;*)

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

let go ui coord gary_coord =
  call_on_timer ui coord;
  loop ui coord

(** tie gary_coord to our gary sprite to see if that works *)
let draw ui matrix coord gary_coord =
  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in

  LTerm_draw.clear ctx;

  let open Sprites in
  let open Qsprite in
  let gary = Sprites.gary ~offsetx:1 ~offsety:1 ctx in
  (*Qsprite.render gary;*)
  Qsprite.move gary 8 8;
  Qsprite.render gary

(*
  LTerm_draw.draw_frame ctx { row1 = 0; col1 = 0; row2 = size.rows; col2 = size.cols } LTerm_draw.Light;
  if size.rows > 2 && size.cols > 2 then begin
    let ctx = LTerm_draw.sub ctx { row1 = 1; col1 = 1; row2 = size.rows - 1; col2 = size.cols - 1 } in
    LTerm_draw.draw_styled ctx coord.row coord.col (eval [B_fg LTerm_style.lblue; S"Move me"; E_fg])
  end*)

let main () =
  Lazy.force LTerm.stdout
  >>= fun term ->

  (* Coordinates of the message. *)
  let coord = ref { row = 0; col = 0 } in
  let gary_coord = ref { row = 0; col = 0 } in

  LTerm_ui.create term (fun ui matrix -> draw ui matrix !coord !gary_coord)
  >>= fun ui ->
  Lwt.finalize (fun () -> go ui coord gary_coord) (fun () -> LTerm_ui.quit ui)

let () = Lwt_main.run (main ())
