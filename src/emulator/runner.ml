open Core_kernel.Std
open Format
open Options
open Bap.Std

let ref_tiles = ref [] (* XXX get rid of it later *)

module Input_loop = struct
  open Lwt
  open LTerm_key

  let run term history send_stream =
    (** Create initial debug state *)
    let state = Debugger.Command_interpreter.create send_stream in

    let rec loop term history state =
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
        loop term history state
      | None -> loop term history state in
    loop term history state
end

module Z80_interpreter_loop = struct
  open Lwt

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

  let storage_of_context ctxt =
    match ctxt#lookup (Z80_env.mem) with
    | Some result ->
      (match Bil.Result.value result with
       | Bil.Mem storage ->
         Some storage
       | _ -> None)
    | _ -> None

  let update_tiles_from_mem options ctxt =
    let storage = storage_of_context ctxt in
    let tiles = Screen.get_tiles options storage in
    (match tiles with
     | Some tiles ->
       (*Screen.print_ascii_screen tiles;*)
       ref_tiles := tiles;
     | None -> ())

  let run refresh_rate_frame options ctxt image term stream =
    LTerm_ui.create term (fun ui matrix -> draw ui matrix !ref_tiles) >>= fun ui ->

    (** Create the interpreter *)
    let interpreter = new Z80_interpreter.z80_interpreter image options in
    let ctxt = Z80_interpreter.set_pc ctxt 0 in

    let rec loop ui ctxt =
      (** Steps a frame *)
      let ctxt' = Z80_interpreter.step_frame options interpreter ctxt image in

      (** RENDER HERE *)
      (** Set tiles from memory *)
      update_tiles_from_mem options ctxt;
      LTerm_ui.draw ui;

      (** Sleep, check input, and loop *)
      Lwt_unix.sleep refresh_rate_frame >>= fun _ ->
      let elems = Lwt_stream.get_available stream in
      match elems with
      | [Debugger.Request.Sleep sleepy] ->
        Lwt_io.printf "[+] Event: Pause/Sleep\n%!" >>= fun () ->
        sleepy >>= fun () ->
        loop ui ctxt'
      | [Debugger.Request.Bp loc] ->
        Lwt_io.printf "[+] Event: BP to set at %d\n%!" loc >>= fun () ->
        loop ui ctxt'
      | _ -> loop ui ctxt' in
    loop ui ctxt
end

let start_event_loop refresh_rate_frame options ctxt image =
  let open Lwt in
  let open LTerm_key in

  (* TODO: mailbox instead of stream? need only one *)
  let recv_stream, send_stream = Lwt_stream.create () in

  Lwt_io.printl "Starting event_loop" >>= fun () ->
  Lazy.force LTerm.stdout >>= fun term ->
  (*check_small_screen ui;*) (* TODO turn on later *)
  Lwt.finalize
    (fun () -> Lwt.join
        [Z80_interpreter_loop.run refresh_rate_frame options ctxt image term recv_stream;
         Input_loop.run term (LTerm_history.create []) send_stream])
    (fun () -> return ()) (* TODO: cleanup. somehow Lterm_ui.quit ui *)

let run options ctxt image =
  try
    Lwt_main.run (start_event_loop options.frame_speed options ctxt image)
  with
  | LTerm_draw.Out_of_bounds ->
    failwith "Rendering is ON, out of bounds!\n"
  | e -> let s = sprintf "%s" @@ Exn.to_string e in
    failwith s
