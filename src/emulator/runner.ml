open Core_kernel.Std
open Format
open Options
open Bap.Std

(** Log layers:
    ev_dbg_rq_recv : a request is received by the debugger loop, which it must process
    ev_cli_rq_snd  : a request is issued from the cli
    ev_int_rq_snd  : a request is issued from the debug interpreter (e.g. bp)
*)

module SM = Monad.State

let ref_tiles = ref [] (* XXX get rid of it later *)

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
        let state',out = Debugger_cli.Command_interpreter.process history state command in
        LTerm.fprintls term (make_output state' out) >>= fun () ->
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

  let draw_bg ctxt tiles =
    Background.from_tile_list tiles ctxt |> Background.render

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
    draw_bg ctxt tiles

  let storage_of_context ctxt =
    let open Option in
    ctxt#lookup Z80_env.mem >>= fun result ->
    match Bil.Result.value result with
    | Bil.Mem storage -> return storage
    | _ -> None

  (* XXX mutable state! *)
  let update_tiles_from_mem options ctxt =
    let storage = storage_of_context ctxt in
    let tiles = Screen.get_tiles options storage in
    (match tiles with
     | Some tiles ->
       (*Screen.print_ascii_screen tiles;*)
       ref_tiles := tiles;
     | None -> ())

  (** A request may be received while input is blocking (paused) or
      non-blocking. Everything stays the same, except:
      1) When blocking, an unrecognized request should not change state
      2) When non-blocking, an unrecognized request should simply step the
      interpreter. Otherwise, each unrecognized command issued will
      "skip" the frame for that cycle. *)
  let handle_request ctxt step_frame step_insn rrender (rq,state) =
    let open Debugger_types.Request in
    match rq,state with
    | Step Frame,_ ->
      Lwt_io.printf "[+] Event: %s\n%!"
      @@ Sexp.to_string (sexp_of_t (Step Frame)) >|= fun () ->
      step_frame ctxt
    | Step Insn,_ ->
      Lwt_io.printf "[+] Event: %s\n%!"
      @@ Sexp.to_string (sexp_of_t (Step Insn)) >|= fun () ->
      step_insn ctxt
    | Print Regs,_ ->
      Lwt_io.printf "[+] Event: (print regs)\n%!" >|= fun () ->
      ctxt#print_cpu;
      ctxt
    | Print Insn,_ ->
      Lwt_io.printf "[+] Event: (print insn)\n%!" >|= fun () ->
      Format.printf "%a\n%!" Z80_disassembler.Hunk.pp ctxt#current_hunk;
      ctxt
    | Bp addr,_ ->
      Lwt_io.printf "[+] Event: place BP 0x%x\n%!" addr >|= fun () ->
      ctxt#add_breakpoint addr
    | Help,_ ->
      Lwt_io.printf "[+] Event: (print insn)\n%!" >|= fun () ->
      let pp rq_variant =
        let rq = rq_variant.Variantslib.Variant.name in
        printf "%s@." rq in
      (* (print insn) *)
      Variants.iter
        ~pause:pp
        ~resume:pp
        ~bp:pp
        ~step:pp
        ~print:pp
        ~help:pp
        ~render:pp;
      ctxt
    | Render,_ ->
      Lwt_io.printf "[+] Event: Force render\n%!" >|= fun () ->
      rrender ctxt;
      ctxt
    | _,`Blocking -> return ctxt (* Any other command in the blocking
                                    state is ctxt *)
    | _,`Non_blocking -> return (step_frame ctxt) (* Any other command
                                                     in the non-blocking
                                                     state is step_frame (why?)*)

  (** Blocking input loop when paused *)
  let blocking_input_loop_on_pause
      recv_stream ctxt step_frame step_insn rrender =
    let open Debugger_types.Request in
    Lwt_io.printf "Paused. Blocking input mode on!\n%!" >>= fun () ->
    Lwt_stream.next recv_stream >>= fun rq ->
    let rec loop_blocking_while_paused rq ctxt =
      match rq with
      | Resume ->
        (* On resume, step_frame or step_insn? step_insn slow for
           'real time'. so that's why i chose step_frame.*)
        Lwt_io.printf "[+] Event: RESUME on block\n%!" >|= fun () ->
        step_insn ctxt
      | _ ->
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
      recv_stream =
    (** Create screen matrix. XXX mutable state *)
    LTerm_ui.create term (fun ui matrix -> draw ui matrix !ref_tiles) >>= fun ui ->

    let rrender ctxt = update_tiles_from_mem options ctxt in

    let rec loop ui ctxt count =
      let open Debugger_types.Request in
      (** Render: Set tiles from memory *)
      (* Hack: only render ever 10k steps *)
      if count mod 10000 = 0 then
        rrender ctxt;
      LTerm_ui.draw ui;


      (** Non-blocking: handle input requests *)
      (match Lwt_stream.get_available recv_stream with
       | [Pause] ->
         (** Enter blocking input mode for stream *)
         let section = Lwt_log.Section.make "ev_dbg_rq_recv" in
         Lwt_log.debug ~section "Pause" >>= fun () ->
         blocking_input_loop_on_pause recv_stream ctxt step_frame step_insn rrender
       | [rq] ->
         let section = Lwt_log.Section.make "ev_dbg_rq_recv" in
         Lwt_log.debug_f ~section
           "%s" @@ Debugger_types.Request.to_string rq >>= fun () ->
         handle_request ctxt step_frame step_insn rrender (rq,`Non_blocking)
       | _ ->
         (** Used to be step_frame, but i'm debugging *)
         (** If empty, steps an insn by default, next ctxt *)
         step_insn ctxt |> return)
      >>= fun ctxt' ->

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

  let stmts =
    if options.bootrom then Boot.clean_state
    else Boot.ready_state in

  let ctxt = new Z80_interpreter_debugger.context image options in
  let ctxt = ctxt#with_pc (Bil.Imm (i16 0)) in
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
  Lwt_log_core.add_rule "ev_*" Lwt_log_core.Debug;
  Lwt_log.default := logger;

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
    (fun () -> Lwt.join
        [interp_loop;input_loop])
    (fun () -> return ()) (* TODO: cleanup. somehow Lterm_ui.quit ui *)

let run options image =
  try
    Lwt_main.run (start_event_loop options.frame_speed options image)
  with
  | LTerm_draw.Out_of_bounds ->
    failwith "Rendering is ON, out of bounds!\n"
  | e -> let s = sprintf "%s" @@ Exn.to_string e in
    failwith s
