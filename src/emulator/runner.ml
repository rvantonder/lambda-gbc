open Core_kernel.Std
open Format
open Options
open Bap.Std

module SM = Monad.State

let start_event_loop refresh_rate_frame options image =
  let open Lwt in
  let open LTerm_key in

  Lwt_log.file ~file_name:"lgbc.log" ~mode:`Truncate () >>= fun logger ->
  (** All interpreter debugger request events under "ev_*" section
      are logged to Debug level. *)
  (*Lwt_log_core.add_rule "ev_*" Lwt_log_core.Debug;*)
  Lwt_log.default := logger;

  (** Stream for sending/receiving commands *)
  let recv_stream, send_stream = Lwt_stream.create () in

  Lwt_log.debug "Starting event_loop" >>= fun () ->
  Lazy.force LTerm.stdout >>= fun term ->

  (*check_small_screen ui;*) (* TODO turn on later *)

  (* If debugging is enabled, pause NOW *)
  (*send_stream (Some Debugger_types.Request.Pause);*)

  let may_continue = Lwt_mvar.create () in
  (* for testing. take the variable so later thread doesn't have opportunity to
     continue*)

  (* TODO return immediately, racy *)
  Lwt_mvar.take may_continue |> fun r -> Lwt.on_termination r ident;

  let interp_loop = Thread_cpu.set_up_debug_interp_loop
      options image term recv_stream send_stream may_continue in

  let input_loop = Thread_debugger.set_up_input_loop term send_stream in

  let clock_loop = Thread_clock.start may_continue in

  Lwt.finalize
    (fun () -> Lwt.join [interp_loop;input_loop;clock_loop])
    (fun () -> return ()) (* TODO: cleanup. somehow Lterm_ui.quit ui *)

let run options image =
  try
    Lwt_main.run (start_event_loop options.frame_speed options image)
  with
  | LTerm_draw.Out_of_bounds ->
    failwith "Rendering is ON, out of bounds!\n"
  | e -> let s = sprintf "%s" @@ Exn.to_string e in
    failwith s
