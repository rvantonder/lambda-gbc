open Core_kernel.Std
open Format
open Options
open Bap.Std

let storage_of_context ctxt =
  match ctxt#lookup (Z80_env.mem) with
  | Some result ->
    (match Bil.Result.value result with
     | Bil.Mem storage ->
       Some storage
     | _ -> None)
  | _ -> None


let ref_tiles = ref [] (* XXX get rid of it later *)

let update_tiles_from_mem options ctxt =
  let storage = storage_of_context ctxt in
  let tiles = Screen.get_tiles options storage in
  (match tiles with
   | Some tiles ->
     (*Screen.print_ascii_screen tiles;*)
     ref_tiles := tiles;
   | None -> ())

let check_small_screen ui =
  let open LTerm_geom in
  let size = LTerm_ui.size ui in
  (if size.rows < 289 || size.cols < 1430 then (* XXX thumb suck *)
     raise (Failure
              "I'm not going to continue drawing. Screen too small"))

let draw_bg ctxt tiles =
  Background.from_tile_list tiles ctxt
  |> Background.render

let draw_gary ctxt =
  let open Sprites in
  let open Qsprite in
  let gary = Sprites.gary ~offsetx:1 ~offsety:1 ctxt in
  Qsprite.move gary 8 8;
  Qsprite.render gary

(** TODO: why if i raise exception here does it get ignored? *)
let draw ui matrix tiles =
  let open LTerm_geom in
  let size = LTerm_ui.size ui in
  let ctxt = LTerm_draw.context matrix size in
  (*Format.printf "Size: %s\n" @@ LTerm_geom.string_of_size size;
    Format.printf "%b %b" (size.rows < 289) (size.cols < 1430);
    (if size.rows < 289 || size.cols < 1430 then
     raise (Failure "I'm not going to continue drawing. Screen too small"));*)
  LTerm_draw.clear ctxt;
  draw_bg ctxt tiles;
  draw_gary ctxt


let event_loop refresh_frame options interpreter ctxt image =
  let open Lwt in
  let open LTerm_key in

  let stream, push = Lwt_stream.create () in

  let rec input_loop term history state =
    let open Debugger.Repl in
    Lwt.catch (fun () ->
        let rl = new read_line ~term
          ~history:(LTerm_history.contents history) ~state in
        rl#run >|= fun command -> Some command) (function
        | Sys.Break -> return None
        | exn -> Lwt.fail exn) >>= function
    | Some command ->
      let state,out = Debugger.Command_interpreter.process state command in
      LTerm.fprintls term (make_output state out) >>= fun () ->
      LTerm_history.add history command;
      input_loop term history state
    | None -> input_loop term history state
  in

  let rec frame_loop term ui ctxt =
    printf "In loop\n%!";
    (** Steps a frame *)
    let ctxt' = Z80_interpreter.step_frame options interpreter ctxt image in

    (** RENDER HERE *)
    (** Set tiles from memory *)
    (*printf "Dumping vram\n%!";
      ctxt#dump_vram;*)
    update_tiles_from_mem options ctxt;
    LTerm_ui.draw ui;

    (** Sleep, check input, and loop *)
    Lwt_unix.sleep refresh_frame >>= fun _ ->
    Lwt_io.printf "Checking stream\n%!" >>= fun () ->
    let elems = Lwt_stream.get_available stream in
    Lwt_io.printf "Size events: %d\n%!" (List.length elems) >>= fun () ->
    match elems with
    | [Debugger.Request.Sleep sleepy] ->
      Lwt_io.printf "PAUSING\n%!" >>= fun () ->
      sleepy >>= fun () ->
      frame_loop term ui ctxt'
    | [Debugger.Request.Bp loc] ->
      Lwt_io.printf "BP to set at %d\n%!" loc >>= fun () ->
      frame_loop term ui ctxt'
    | _ -> frame_loop term ui ctxt' in

  Lwt_io.printl "Starting event_loop" >>= fun () ->
  Lazy.force LTerm.stdout >>= fun term ->
  update_tiles_from_mem options ctxt; (* TODO probably safe to remove *)
  LTerm_ui.create term (fun ui matrix -> draw ui matrix !ref_tiles) >>= fun ui ->
  (*check_small_screen ui;*) (* TODO turn on later *)
  let state = Debugger.Command_interpreter.create push in
  Lwt.finalize
    (fun () -> Lwt.join
        [frame_loop term ui ctxt;
         input_loop term (LTerm_history.create []) state])
    (fun () -> LTerm_ui.quit ui)

let run options ctxt image =
  let interpreter = new Z80_interpreter.z80_interpreter image options in
  let ctxt' = Z80_interpreter.set_pc ctxt 0 in
  try Lwt_main.run (event_loop options.frame_speed options interpreter ctxt' image)
  with
  | LTerm_draw.Out_of_bounds ->
    failwith "Rendering is ON, out of bounds!\n"
  | e -> let s = sprintf "%s" @@ Exn.to_string e in
    failwith s
