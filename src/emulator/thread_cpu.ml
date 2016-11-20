open Core_kernel.Std
open Options
open Bap.Std

module SM = Monad.State

open Logging

module Z80_interpreter_loop = struct
  open Lwt

  let check_small_screen ui =
    let open LTerm_geom in
    let size = LTerm_ui.size ui in
    (if size.rows < 289 || size.cols < 1430 then (* XXX thumb suck *)
       raise (Failure "I'm refuse to continue drawing. Screen too small"))

  let draw_bg ctxt lterm_ctxt tiles =
    log_render "Drawing bg";
    let scroll_offset addr =
      let addr = Addr.of_int ~width:16 addr in
      let value = ctxt#mem_at_addr addr in
      match value with
      | Some v ->
        Some (Word.to_int v |> Or_error.ok_exn)
      | _ ->
        None in
    let scroll_offset_y = scroll_offset 0xFF42 in
    let scroll_offset_x = scroll_offset 0xFF43 in
    Background.from_tile_list
      ?offset_y:scroll_offset_y
      ?offset_x:scroll_offset_x
      tiles lterm_ctxt |>
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
    let res =
      Lwt_stream.next draw_recv_stream in
    Lwt.on_success res (fun (ctxt,finished_drawing) ->
        log_render "Received ctxt to draw";
        (match tiles_from_mem ctxt with
         | Some tiles ->
           let size = LTerm_ui.size ui in
           let lterm_ctxt = LTerm_draw.context matrix size in
           (*Format.printf "Size: %s\n" @@ LTerm_geom.string_of_size size;
             Format.printf "%b %b" (size.rows < 289) (size.cols < 1430);
             (if size.rows < 289 || size.cols < 1430 then
              raise (Failure "I'm not going to continue drawing. Screen too small"));*)
           log_render "Clearing lterm";
           LTerm_draw.clear lterm_ctxt;
           draw_bg ctxt lterm_ctxt tiles;
         | None -> ());
        Lwt.on_success (Lwt_mvar.put finished_drawing ()) (fun _ ->
            log_render "Finished drawing";)
      )

  (** A request may be received while input is blocking (paused) or
      non-blocking. Everything stays the same, except:
      1) When blocking, an unrecognized request should not change state
      2) When non-blocking, an unrecognized request should simply step the
      interpreter. Otherwise, each unrecognized command issued will
      "skip" the frame for that cycle. *)
  let handle_request ctxt step_frame step_insn rrender (rq,state)
    : Z80_interpreter_debugger.context Lwt.t =
    let open Debugger_types.Request in
    (match state with
     | `Blocking -> log_ev_dbg_rq_rcv "handle rq BLOCKING"
     | `Non_blocking -> log_ev_dbg_rq_rcv "handle rq NON-BLOCKING");

    log_ev_dbg_rq_rcv
      @@ sprintf "Event %s" @@ Sexp.to_string (sexp_of_t rq);
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

    log_ev_dbg_rq_rcv_in_blking "MODE ACTIVE: BLOCKING";
    Lwt_io.printf "Paused. Blocking input mode on!\n%!" >>= fun () ->
    log_ev_dbg_rq_rcv_in_blking "Paused. Blocking input mode on!";

    Lwt_stream.next recv_stream >>= fun rq ->
    let rec loop_blocking_while_paused rq ctxt =
      match rq with
      | Resume ->
        (* On resume, step_frame or step_insn? step_insn slow for
           'real time'. so that's why i chose step_frame.*)
        log_ev_dbg_rq_rcv_in_blking "Resume";
        Lwt_io.printf "[+] Event: RESUME on block\n%!" >|= fun () ->
        (** I don't think we should step here. just return ctxt *)
        ctxt
      | rq ->
        log_ev_dbg_rq_rcv_in_blking @@
        Debugger_types.Request.to_string rq;
        handle_request ctxt step_frame step_insn rrender (rq,`Blocking) >>= fun ctxt ->
        Lwt_stream.next recv_stream >>= fun rq ->
        loop_blocking_while_paused rq ctxt in
    loop_blocking_while_paused rq ctxt

  let handle_debug_rq_or_step_once cmd_recv_stream ctxt step_frame step_insn
      rrender =
    let open Debugger_types.Request in
    match Lwt_stream.get_available cmd_recv_stream with
    | [Pause] ->
      (** Enter blocking input mode for stream *)
      log_ev_dbg_rq_rcv_in_non_blking "Pause";
      blocking_input_loop_on_pause
        cmd_recv_stream ctxt step_frame step_insn rrender
    | [rq] ->
      log_ev_dbg_rq_rcv_in_non_blking
      @@ Debugger_types.Request.to_string rq;
      handle_request ctxt step_frame step_insn rrender (rq,`Non_blocking)
    | [] -> return (step_insn ctxt)
    | _ ->
      (*Lwt_log.ign_fatal ~section:section_dbg
        "Do not handle more than one event!";*)
      failwith "Bad: more than one rq in queue"


  (** Design: the interpreter can only be advanced by step_insn and step_frame
      in run. Nothing else about the interpreter is exposed *)
  let run
      interp
      step_insn
      step_frame
      options ctxt
      image
      term
      cmd_recv_stream
      may_continue
    =

    (* Fuck mutable coords/ctxt*)
    let draw_recv_stream, draw_send_stream = Lwt_stream.create () in
    let finished_drawing = Lwt_mvar.create () in
    (* remove the default variable from finished_drawing *)
    Lwt_mvar.take finished_drawing >>= fun _ ->

    (** Create screen matrix. XXX mutable state *)
    LTerm_ui.create term (fun ui matrix -> draw draw_recv_stream ui matrix)
    >>= fun ui ->

    let rrender ctxt =
      (* send ctxt to inside ui. it will wait there until draw ui is called
         below *)
      log_render "Sending ctxt";
      draw_send_stream (Some (ctxt, finished_drawing));
      (* call it *)
      log_render "LTerm_ui.draw";
      (* now immediately it calls draw and returns, not waiting*)
      LTerm_ui.draw ui
    in

    let rec update ui ctxt cycles_done =
      log_ev_dbg_rq_rcv_in_non_blking "MODE ACTIVE: NON-BLOCKING";

      (* Non-blocking: handle input requests, or step *)
      handle_debug_rq_or_step_once
        cmd_recv_stream ctxt step_frame
        step_insn
        rrender
      >>= fun ctxt' ->
      let cycles_delta = ctxt'#cpu_clock - ctxt#cpu_clock in
      let cycles_done = cycles_done + cycles_delta in

      Gpu.update interp ctxt' cycles_delta |> fun ctxt' ->

      if cycles_done >= 70244
      (* may_continue is a synchronising variable. We put something into
         may_continue at 60hz, and when that something is there, this loop
         can continue. If there's nothing there, it has to block and wait *)
      then
        (log_render "Cycles done. Doing hard render";
         rrender ctxt';
         Lwt_mvar.take finished_drawing >>= fun _ ->

         log_cycles @@
         sprintf "Waiting to continue. Cycles: %d" cycles_done;

         Lwt_mvar.take may_continue >>= fun _ ->
         update ui ctxt' (cycles_done - 70244)
        )
        (* XXX : waiting stops the whole debug loop. another reason
           to pull it out *)
        (* not 0, but including the cycles if we went past *)
      else
        (log_cycles @@ sprintf "Cycles: %d" cycles_done;
         update ui ctxt' cycles_done)
    in

    update ui ctxt 0
end

(** Later, recv_stream here will be input *)
let create_base_interp_loop
    interp
    ctxt
    options
    image
    term
    recv_stream
    may_continue
  =

  let step_frame ctxt = SM.exec interp#step_frame ctxt in
  let step_insn ctxt = SM.exec interp#step_insn ctxt in
  Z80_interpreter_loop.run interp step_insn step_frame options ctxt
    image term recv_stream may_continue

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
let set_up_debug_interp_loop options
    image term recv_stream send_stream
    may_continue =

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

  create_base_interp_loop
    interp ctxt options image term recv_stream may_continue
