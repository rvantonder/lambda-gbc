

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



let set_up_input_loop term send_stream =
  Input_loop.run term (LTerm_history.create []) send_stream
