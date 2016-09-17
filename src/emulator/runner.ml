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
        (* Parse and send the command *)
        let state',out = Debugger.Command_interpreter.process history state command in
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
  let handle_request ctxt step_frame step_insn (rq,state) =
    let open Debugger.Request in
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
    | Help,_ ->
      Lwt_io.printf "[+] Event: (print insn)\n%!" >|= fun () ->
      let pp rq_variant =
        let rq = rq_variant.Variantslib.Variant.name in
        printf "%s@." rq in
      (* (print insn) *)
      Variants.iter ~pause:pp ~resume:pp ~bp:pp ~step:pp ~print:pp ~help:pp;
      ctxt
    | _,`Blocking -> return ctxt (* Any other command in the blocking
                                    state is ctxt *)
    | _,`Non_blocking -> return (step_frame ctxt) (* Any other command
                                                     in the non-blocking
                                                     state is step_frame (why?)*)

  (** Blocking input loop when paused *)
  let blocking_input_loop_on_pause recv_stream ctxt step_frame step_insn =
    let open Debugger.Request in
    Lwt_io.printf "Paused. Blocking input mode on!\n%!" >>= fun () ->
    Lwt_stream.next recv_stream >>= fun rq ->
    let rec loop_blocking_while_paused rq ctxt =
      match rq with
      | Resume ->
        (* On resume, step_frame or step_insn? step_insn slow for
           'real time'. so that's why i chose step_frame.*)
        Lwt_io.printf "[+] Event: RESUME on block\n%!" >|= fun () ->
        step_frame ctxt
      | _ ->
        handle_request ctxt step_frame step_insn (rq,`Blocking) >>= fun ctxt ->
        Lwt_stream.next recv_stream >>= fun rq ->
        loop_blocking_while_paused rq ctxt in
    loop_blocking_while_paused rq ctxt

  let run refresh_rate_frame options ctxt image term recv_stream =
    (** Create screen matrix. XXX mutable state *)
    LTerm_ui.create term (fun ui matrix -> draw ui matrix !ref_tiles) >>= fun ui ->

    (** Create the interpreter *)
    let interpreter = new Z80_interpreter.z80_interpreter image options in

    let rec loop ui ctxt =
      let open Debugger.Request in
      (** Render: Set tiles from memory *)
      update_tiles_from_mem options ctxt;
      LTerm_ui.draw ui;

      let step_frame ctxt =
        Z80_interpreter.step_frame options interpreter ctxt image in

      let step_insn ctxt =
        Z80_interpreter.step_insn options interpreter ctxt image in

      (** Non-blocking: handle input requests *)
      (match Lwt_stream.get_available recv_stream with
       | [Pause] ->
         (** Enter blocking input mode for stream *)
         blocking_input_loop_on_pause recv_stream ctxt step_frame step_insn
       | [rq] -> handle_request ctxt step_frame step_insn (rq,`Non_blocking)
       | _ ->
         (** If empty, steps a frame by default, next ctxt *)
         step_frame ctxt |> return)
      >>= fun ctxt' ->

      (** Sleep *)
      Lwt_unix.sleep refresh_rate_frame >>= fun _ ->
      loop ui ctxt' in

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
