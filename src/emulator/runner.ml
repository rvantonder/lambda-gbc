open Core_kernel.Std
open Format
open Options
open Bap.Std

(** Log layers:
    ev_dbg_rq_rcv : a request is received by the debugger loop, which it must process
    ev_cli_rq_snd  : a request is issued from the cli
    ev_int_rq_snd  : a request is issued from the debug interpreter (e.g. bp)
*)

module SM = Monad.State

module Input_loop = struct
  open Lwt
  open LTerm_key

  let run term history send_stream =
    (** Create initial debug state *)
    let state = Debugger_cli.Command_interpreter.create send_stream in

    let rec loop term history state =
      let open Debugger_cli.Repl in
      Lwt.catch (fun () ->
          let rl = new read_line ~term
            ~history:(LTerm_history.contents history) ~state in
          rl#run >|= fun command -> Some command) (function
          | Sys.Break -> return None
          | exn -> Lwt.fail exn) >>= function
      | Some command ->
        (* Parse and send the command *)
        let state',out = Debugger_cli.Command_interpreter.process
            history state command in
        (* Don't really need CLI to dump output here*)
        (*LTerm.fprintls term (make_output state' out) >>= fun () ->*)
        LTerm_history.add history command;
        loop term history state'
      | None -> loop term history state in
    loop term history state
end

module Z80_interpreter_loop = struct
  open Lwt

  let check_small_screen ui =
    let open LTerm_geom in
    let size = LTerm_ui.size ui in
    (if size.rows < 289 || size.cols < 1430 then (* XXX thumb suck *)
       raise (Failure "I'm refuse to continue drawing. Screen too small"))

  let draw_bg ctxt lterm_ctxt tiles =
    let offset addr =
      let addr = Addr.of_int ~width:16 addr in
      let value = ctxt#mem_at_addr addr in
      match value with
      | Some v ->
        Some (Word.to_int v |> Or_error.ok_exn)
      | _ ->
        None in
    let offset_y = offset 0xFF42 in
    let offset_x = offset 0xFF43 in
    Background.from_tile_list ?offset_y ?offset_x tiles lterm_ctxt |>
    Background.render

  let storage_of_context ctxt =
    let open Option in
    ctxt#lookup Z80_env.mem >>= fun result ->
    match Bil.Result.value result with
    | Bil.Mem storage -> return storage
    | _ -> None

  let tiles_from_mem ctxt =
    let storage = storage_of_context ctxt in
    Screen.get_tiles storage
    (*let tiles = Screen.get_tiles options storage in
    (match tiles with
     | Some tiles ->
       (*Screen.print_ascii_screen tiles;*)
       ref_tiles := tiles;
     | None -> ())*)

  (** TODO: why if i raise exception here does it get ignored? *)
  let draw draw_recv_stream ui matrix : unit =
    let open LTerm_geom in
    let ctxt = Lwt_stream.next draw_recv_stream in (* TODO: how to return unit when reading from lwt here? *)
    Lwt.on_success ctxt (fun ctxt ->
        printf "On_success_triggered";
        match tiles_from_mem ctxt with
        | Some tiles ->
          let size = LTerm_ui.size ui in
          let lterm_ctxt = LTerm_draw.context matrix size in
          (*Format.printf "Size: %s\n" @@ LTerm_geom.string_of_size size;
            Format.printf "%b %b" (size.rows < 289) (size.cols < 1430);
            (if size.rows < 289 || size.cols < 1430 then
             raise (Failure "I'm not going to continue drawing. Screen too small"));*)
          LTerm_draw.clear lterm_ctxt;
          draw_bg ctxt lterm_ctxt tiles
        | None -> ())

  (** A request may be received while input is blocking (paused) or
      non-blocking. Everything stays the same, except:
      1) When blocking, an unrecognized request should not change state
      2) When non-blocking, an unrecognized request should simply step the
      interpreter. Otherwise, each unrecognized command issued will
      "skip" the frame for that cycle. *)
  let handle_request ctxt step_frame step_insn rrender (rq,state)
    : Z80_interpreter_debugger.context Lwt.t =
    let open Debugger_types.Request in
    let section = Lwt_log.Section.make "ev_dbg_rq_rcv" in
    (match state with
     | `Blocking -> Lwt_log.ign_debug ~section "handle rq BLOCKING"
     | `Non_blocking -> Lwt_log.ign_debug ~section "handle rq NON-BLOCKING");

    Lwt_log.ign_debug_f ~section "Event %s" @@ Sexp.to_string (sexp_of_t rq);
    let ctxt' =
      match rq,state with
      | Step Frame,_ -> step_frame ctxt
      | Step Insn,_ ->
        let ctxt = step_insn ctxt in
        (** PC is updated, but current hunk is not yet. So hit decode to get
            the current hunk *)
        let hunk : Z80_disassembler.Hunk.t = (ctxt#decode)#current_hunk in
        Format.printf "\n%!";
        Format.printf "%a\n%!" Z80_disassembler.Hunk.pp hunk;
        ctxt
      | Print Regs,_ ->
        ctxt#print_cpu;
        ctxt
      | Print Insn,_ ->
        (** PC is updated, but current hunk is not yet. So hit decode to get
            the current hunk *)
        let hunk : Z80_disassembler.Hunk.t = (ctxt#decode)#current_hunk in
        Format.printf "\n%!";
        Format.printf "%a\n%!" Z80_disassembler.Hunk.pp hunk;
        ctxt
      | Bp addr,_ ->
        ctxt#add_breakpoint addr
      | Render,_ ->
        rrender ctxt;
        ctxt
      | Print Mem w,_ ->
        let addr = Addr.of_int ~width:16 w in
        let value = ctxt#mem_at_addr addr in
        Format.printf "\n%!";
        (match value with
         | Some v -> Format.printf "0x%x\n%!" @@
           (Word.to_int v |> Or_error.ok_exn)
         | None -> Format.printf "_|_\n%!");
        ctxt
      | Help,_ ->
        let pp rq_variant =
          let rq = rq_variant.Variantslib.Variant.name in
          printf "%s@." rq in
        Format.printf "\n%!";
        Variants.iter ~pause:pp ~resume:pp ~bp:pp ~step:pp ~print:pp ~help:pp
          ~render:pp;
        ctxt
      (** Any other command in the blocking state is ctxt *)
      | _,`Blocking -> ctxt
      (** Any other command in the non-blocking state is step_frame (why?)*)
      | _,`Non_blocking -> (step_frame ctxt) in
    return ctxt'

  (** Blocking input loop when paused *)
  let blocking_input_loop_on_pause
      recv_stream ctxt step_frame step_insn rrender =
    let open Debugger_types.Request in

    let section = Lwt_log.Section.make "ev_dbg_rq_rcv_in_blking" in
    Lwt_log.debug ~section "MODE ACTIVE: BLOCKING" >>= fun () ->

    Lwt_io.printf "Paused. Blocking input mode on!\n%!" >>= fun () ->
    Lwt_log.debug ~section "Paused. Blocking input mode on!" >>= fun () ->
    Lwt_stream.next recv_stream >>= fun rq ->
    let rec loop_blocking_while_paused rq ctxt =
      match rq with
      | Resume ->
        (* On resume, step_frame or step_insn? step_insn slow for
           'real time'. so that's why i chose step_frame.*)
        Lwt_log.debug ~section "Resume" >>= fun () ->
        Lwt_io.printf "[+] Event: RESUME on block\n%!" >|= fun () ->
        (** I don't think we should step here. just return ctxt *)
        ctxt
      | rq ->
        Lwt_log.debug_f ~section
          "%s" @@ Debugger_types.Request.to_string rq >>= fun () ->
        handle_request ctxt step_frame step_insn rrender (rq,`Blocking) >>= fun ctxt ->
        Lwt_stream.next recv_stream >>= fun rq ->
        loop_blocking_while_paused rq ctxt in
    loop_blocking_while_paused rq ctxt

  (** Design: the interpreter can only be advanced by step_insn and step_frame
      in run. Nothing else about the interpreter is exposed *)
  let run
      step_insn
      step_frame
      refresh_rate_frame
      options ctxt
      image
      term
      cmd_recv_stream =

    let section = Lwt_log.Section.make "ev_dbg_rq_rcv_in_non_blking" in

    (* Fuck mutable coords/ctxt*)
    let draw_recv_stream, draw_send_stream = Lwt_stream.create () in

    (** Create screen matrix. XXX mutable state *)
    LTerm_ui.create term (fun ui matrix -> draw draw_recv_stream ui matrix) >>= fun ui ->

    let rrender ctxt =
      draw_send_stream (Some ctxt);
      LTerm_ui.draw ui
    in

    let rec loop ui ctxt count =
      let open Debugger_types.Request in
      Lwt_log.debug ~section "MODE ACTIVE: NON-BLOCKING" >>= fun () ->

      (** Render: Set tiles from memory *)
      (* Hack: only render ever 10k steps *)
      (*if count mod 10000 = 0 then*)
      if not options.no_render then
        (rrender ctxt;
         LTerm_ui.draw ui);

      (** Non-blocking: handle input requests *)
      (match Lwt_stream.get_available cmd_recv_stream with
       | [Pause] ->
         (** Enter blocking input mode for stream *)
         Lwt_log.debug ~section "Pause" >>= fun () ->
         blocking_input_loop_on_pause cmd_recv_stream ctxt step_frame step_insn rrender
       | [rq] ->
         Lwt_log.debug_f ~section
           "%s" @@ Debugger_types.Request.to_string rq >>= fun () ->
         handle_request ctxt step_frame step_insn rrender (rq,`Non_blocking)
       | [] ->
         Lwt_log.debug ~section:Lwt_log.Section.main "step" >>= fun () ->
         (** do not log here... empty polling *)
         (** Used to be step_frame, but i'm debugging *)
         (** If empty, steps an insn by default, next ctxt *)
         step_insn ctxt |> return
       | _ ->
         Lwt_log.ign_fatal ~section "Do not handle more than one event!";
         failwith "Bad: more than one rq in queue"
      ) >>= fun ctxt' ->

      (** Sleep *)
      Lwt_unix.sleep refresh_rate_frame >>= fun _ ->
      loop ui ctxt' (count + 1) in

    loop ui ctxt 0
end

let set_up_input_loop term send_stream =
  Input_loop.run term (LTerm_history.create []) send_stream

let set_up_base_interp_loop
    interp
    ctxt
    refresh_rate_frame
    options
    image
    term
    recv_stream =

  let step_frame ctxt = SM.exec interp#step_frame ctxt in
  let step_insn ctxt = SM.exec interp#step_insn ctxt in
  Z80_interpreter_loop.run step_insn step_frame refresh_rate_frame options ctxt
    image term recv_stream

let i16 = Word.of_int ~width:16

(* TODO: make sure the command thing is only enabled for debugger 'view'. This doesn't type check otherwise
   let set_up_interp_loop
   refresh_rate_frame
   options ctxt
   image term recv_stream =

   let interp =
   new Z80_interpreter.z80_interpreter image options in

   let stmts =
   if options.bootrom then Boot.clean_state
   else Boot.ready_state in

   let ctxt = new Z80_interpreter.context image options in
   let ctxt = ctxt#with_pc (Bil.Imm (i16 0)) in
   let start = interp#eval stmts in

   let ctxt = Monad.State.exec start ctxt in

   set_up_base_interp_loop
   interp ctxt refresh_rate_frame options image term recv_stream
*)

(** The difference between the normal interpreter and debug interpreter is that
    we give the debug interpreter the send_event_stream. The same send
    event_stream that the user publishes on. This is so that the debugger itself
    can issue requests such as "Pause" when a breakpoint is hit. Worth asking if
    it should have its own debug stream.
*)
let set_up_debug_interp_loop refresh_rate_frame options
    image term recv_stream send_stream =

  let interp =
    new Z80_interpreter_debugger.z80_interpreter_debugger image options
      send_stream in

  let init_pc,stmts =
    if options.bootrom then 0x0,Boot.clean_state
    else 0x100,Boot.ready_state in

  let ctxt = new Z80_interpreter_debugger.context image options in
  let ctxt = ctxt#with_pc (Bil.Imm (i16 init_pc)) in
  let start = interp#eval stmts in

  let ctxt = Monad.State.exec start ctxt in

  set_up_base_interp_loop
    interp ctxt refresh_rate_frame options image term recv_stream


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
  send_stream (Some Debugger_types.Request.Pause);

  let interp_loop = set_up_debug_interp_loop refresh_rate_frame
      options image term recv_stream send_stream in

  let input_loop = set_up_input_loop term send_stream in

  (*Lwt_log.ign_debug "Spinning up event loops...";*)
  Lwt.finalize
    (fun () -> Lwt.join [interp_loop;input_loop])
    (fun () -> return ()) (* TODO: cleanup. somehow Lterm_ui.quit ui *)

let run options image =
  try
    Lwt_main.run (start_event_loop options.frame_speed options image)
  with
  | LTerm_draw.Out_of_bounds ->
    failwith "Rendering is ON, out of bounds!\n"
  | e -> let s = sprintf "%s" @@ Exn.to_string e in
    failwith s
