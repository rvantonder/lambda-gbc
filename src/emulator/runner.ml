open Core_kernel.Std
open Format
open Options
open Bap.Std

module SM = Monad.State

let start_event_loop refresh_rate_frame options image =
  let open Lwt in
  let open LTerm_key in

  Lwt_log.file ~file_name:"lgbc.log" ~mode:`Truncate () >>= fun logger ->
  Lwt_log.default := logger;

  (** Stream for sending/receiving debug commands *)
  let recv_stream, send_stream = Lwt_stream.create () in

  Lwt_log.debug "Starting event_loop" >>= fun () ->
  Lazy.force LTerm.stdout >>= fun term ->

  (*check_small_screen ui;*) (* TODO turn on later *)

  (* If debugging is enabled, pause NOW *)
  (*send_stream (Some Debugger_types.Request.Pause);*)

  (* For testing. take the variable so later thread doesn't have opportunity to
     continue*)
  let may_continue = Lwt_mvar.create () in
  Lwt_mvar.take may_continue
  |> Fn.flip Lwt.on_termination @@ ident;

  let interp_loop =
    Thread_cpu.set_up_debug_interp_loop
      options
      image
      term
      recv_stream
      send_stream
      may_continue
  in
  let input_loop = Thread_debugger.set_up_input_loop term send_stream in
  let clock_loop = Thread_clock.start may_continue in

  Lwt.finalize
    (fun () -> Lwt.join [interp_loop; (*input_loop; clock_loop*)])
    (fun () -> return ())

let run options image =
  try Lwt_main.run (start_event_loop options.frame_speed options image) with
  | LTerm_draw.Out_of_bounds -> failwith "Rendering is ON, out of bounds!\n"
  | e -> failwith @@ sprintf "%s" @@ Exn.to_string e
