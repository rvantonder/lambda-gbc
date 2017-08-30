open Core_kernel.Std
open Options
open Bap.Std
open Util.Util_word

module SM = Monad.State

open Logging

module Z80_interpreter_loop = struct
  open Lwt

  let check_small_screen ui =
    let open LTerm_geom in
    let size = LTerm_ui.size ui in
    if size.rows < 289 || size.cols < 1430 then (* XXX thumb suck *)
      raise (Failure "I refuse to continue drawing. Screen too small")

  (** A request may be received while input is blocking (paused) or
      non-blocking. Everything stays the same, except:
      1) When blocking, an unrecognized request should not change state
      2) When non-blocking, an unrecognized request should simply step the
      interpreter. Otherwise, each unrecognized command issued will
      "skip" the frame for that cycle. *)
  let handle_request ctxt step_frame step_insn screen (request, state)
    : Z80_interpreter_debugger.context Lwt.t =
    let open Debugger_types.Request in
    begin match state with
      | `Blocking -> log_ev_dbg_rq_rcv "handle rq BLOCKING"
      | `Non_blocking -> log_ev_dbg_rq_rcv "handle rq NON-BLOCKING"
    end;

    log_ev_dbg_rq_rcv @@ sprintf "Event %s" @@ Sexp.to_string (sexp_of_t request);
    let ctxt' =
      match request, state with
      | Step Frame, _ -> step_frame ctxt
      | Step Insn, _ ->
        let ctxt = step_insn ctxt in
        (* PC is updated, but current hunk is not yet. So hit decode to get the
            current hunk *)
        let hunk : Hunk.t = (ctxt#decode)#current_hunk in
        Format.printf "@.%a@." Hunk.pp hunk;
        ctxt
      | Print Regs, _ ->
        ctxt#print_cpu;
        ctxt
      | Print Insn, _ ->
        (* PC is updated, but current hunk is not yet. So hit decode to get the
            current hunk *)
        let hunk : Hunk.t = (ctxt#decode)#current_hunk in
        Format.printf "\n%!";
        Format.printf "%a\n%!" Hunk.pp hunk;
        ctxt
      | Bp addr, _ ->
        ctxt#add_breakpoint addr
      | Render, _ ->
        (*Screen.render screen ctxt |> ignore;*) (* TODO fix *)
        ctxt
      | Print Mem w, _ ->
        let addr = Addr.of_int ~width:16 w in
        let value = ctxt#mem_at_addr addr in
        Format.printf "@.";
        begin match value with
          | Some v -> Format.printf "0x%x@." @@
            (Word.to_int v |> Or_error.ok_exn)
          | None -> Format.printf "_|_\n%!"
        end;
        ctxt
      | Help, _ ->
        let pp request =
          let request = request.Variantslib.Variant.name in
          printf "%s@." request
        in
        Format.printf "@.";
        Variants.iter
          ~pause:pp
          ~resume:pp
          ~bp:pp
          ~step:pp
          ~print:pp
          ~help:pp
          ~render:pp;
        ctxt
      (* Any other command in the blocking state is ctxt *)
      | _, `Blocking -> ctxt
      (* Any other command in the non-blocking state is step_frame (why?)*)
      | _, `Non_blocking -> step_frame ctxt
    in
    return ctxt'

  (* Blocking input loop when paused *)
  let blocking_input_loop_on_pause
      recv_stream ctxt step_frame step_insn screen =
    let open Debugger_types.Request in

    log_ev_dbg_rq_rcv_in_blking "MODE ACTIVE: BLOCKING";
    Lwt_io.printf "Paused. Blocking input mode on!\n%!" >>= fun () ->
    log_ev_dbg_rq_rcv_in_blking "Paused. Blocking input mode on!";

    Lwt_stream.next recv_stream >>= fun request ->
    let rec loop_blocking_while_paused request ctxt =
      match request with
      | Resume ->
        (* On resume, step_frame or step_insn? step_insn slow for
           'real time'. so that's why i chose step_frame.*)
        log_ev_dbg_rq_rcv_in_blking "Resume";
        Lwt_io.printf "[+] Event: RESUME on block\n%!" >|= fun () ->
        (** I don't think we should step here. just return ctxt *)
        ctxt
      | request ->
        log_ev_dbg_rq_rcv_in_blking @@
        Debugger_types.Request.to_string request;
        handle_request ctxt step_frame step_insn screen (request, `Blocking)
        >>= fun ctxt ->
        Lwt_stream.next recv_stream
        >>= fun request ->
        loop_blocking_while_paused request ctxt in
    loop_blocking_while_paused request ctxt

  let handle_debug_rq_or_step_once cmd_recv_stream ctxt step_frame step_insn
      screen =
    let open Debugger_types.Request in
    match Lwt_stream.get_available cmd_recv_stream with
    | [Pause] ->
      (* Enter blocking input mode for stream *)
      log_ev_dbg_rq_rcv_in_non_blking "Pause";
      blocking_input_loop_on_pause
        cmd_recv_stream
        ctxt
        step_frame
        step_insn
        screen
    | [request] ->
      log_ev_dbg_rq_rcv_in_non_blking
      @@ Debugger_types.Request.to_string request;
      handle_request ctxt step_frame step_insn screen (request, `Non_blocking)
    | [] -> return (step_insn ctxt)
    | _ ->
      Lwt_log.ign_fatal
        ~section:(Lwt_log.Section.make "fatal")
        "Do not handle more than one event!";
      failwith "Bad: more than one request in queue"


  let run
      interp
      step_insn
      step_frame
      options
      ctxt
      image
      term
      cmd_recv_stream
      may_continue =

    let screen : Screen.t = Screen.create may_continue term in

    let rec update ctxt cycles_done =
      (* debug prompt *)
      (*handle_debug_rq_or_step_once
        cmd_recv_stream ctxt step_frame
        step_insn
        screen*)
      Lwt.return (step_insn ctxt) >>= fun ctxt' ->
      let cycles_delta = ctxt'#cpu_clock - ctxt#cpu_clock in
      let cycles_done = cycles_done + cycles_delta in

      let ctxt' = Gpu.update ctxt' cycles_delta in

      if cycles_done >= 70244
      (* may_continue is a synchronising variable. We put something into
         may_continue at 60hz, and when that something is there, this loop
         can continue. If there's nothing there, it has to block and wait *)
      then begin
        begin match options.no_render with
          | false -> Screen.render screen ctxt' may_continue >>= Lwt.return
          | true -> Lwt.return ()
        end >>= fun () ->
        Lwt_mvar.take may_continue >>= fun _ ->
        update ctxt' (cycles_done - 70244)
      end
      else update ctxt' cycles_done
    in
    update ctxt 0
end

(** Later, recv_stream here will be input *)
let create_base_interp_loop
    interp
    ctxt
    options
    image
    term
    recv_stream
    may_continue =
  let step_frame ctxt = SM.exec interp#step_frame ctxt in
  let step_insn ctxt = SM.exec interp#step_insn ctxt in
  Z80_interpreter_loop.run
    interp
    step_insn
    step_frame
    options
    ctxt
    image
    term
    recv_stream
    may_continue

(** The difference between the normal interpreter and debug interpreter is that
    we give the debug interpreter the send_event_stream. The same send
    event_stream that the user publishes on. This is so that the debugger itself
    can issue requests such as "Pause" when a breakpoint is hit. Worth asking if
    it should have its own debug stream.
*)
let set_up_debug_interp_loop
    options
    image
    term
    recv_stream
    send_stream
    may_continue =

  let interp =
    new Z80_interpreter_debugger.z80_interpreter_debugger
      image
      options
      send_stream
  in

  let init_pc, stmts =
    if options.bootrom then 0x0,Boot.clean_state
    else 0x100,Boot.ready_state in

  let ctxt = new Z80_interpreter_debugger.context image options in
  let ctxt = ctxt#with_pc (Bil.Imm (w16 init_pc)) in
  let start = interp#eval stmts in

  let ctxt = Monad.State.exec start ctxt in

  create_base_interp_loop interp ctxt options image term recv_stream may_continue
