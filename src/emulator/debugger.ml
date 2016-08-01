open React
open Core_kernel.Std
open Sexplib

(** A module for a daemon that interprets commands *)
module Command_interpreter = struct
  type state = { n : int;
                 push_channel : (unit Lwt.t option -> unit) option;
                 resume_after_pause : unit Lwt.u option}

  type command = Pause
               | Resume
               | Bp of int [@@deriving sexp]

  let process state (cmd : string) =
    let state' =
      try
        (String.capitalize cmd |> Sexp.of_string |> command_of_sexp
         |> function
         | Pause -> printf "Pause command!\n%!";
           let sleepy,wakey = Lwt.wait () in
           printf "Pushing sleepy!\n%!";
           (match state.push_channel with
            | Some push ->
              push (Some sleepy);
              {state with resume_after_pause = Some wakey}
            | None -> state)
         | Resume -> printf "Resume command!\n%!";
           (match state.resume_after_pause with
            | Some wakey -> Lwt.wakeup wakey (); state
            | None -> state)
         | Bp pos -> printf "Breakpoint set @ %d\n%!" pos; state)
      with
      | Sexplib.Conv.Of_sexp_error (exn,sexp) -> state (*cmd could not be parsed *)
      | exn ->
        let s = Exn.to_string exn in
        printf "Uncaught exception in Debugger_command.ml: %s\n%!" s;
        state
    in
    let out = "evaluated " ^ cmd in
    let new_state = { state' with n = state'.n + 1 } in
    (new_state, out)

end

module Repl = struct

  (** When using dot notation for accesing record fields, we can
      qualify the field by the module directly
      https://realworldocaml.org/v1/en/html/records.html. Only for
      record types. Not function. *)
  (* Create a prompt based on the current interpreter state *)
  let make_prompt state =
    (* let open Command_interpreter in *)
    let prompt = Printf.sprintf "In  [%d]: " state.Command_interpreter.n in
    LTerm_text.eval [ LTerm_text.(S prompt) ]

  (* Format the interpreter output for REPL display *)
  let make_output state out =
    let open LTerm_text in
    let output = Printf.sprintf "Out [%d]: %s" state.Command_interpreter.n out in
    LTerm_text.eval [ LTerm_text.(S output) ]

  class read_line ~term ~history ~state = object(self)
    inherit LTerm_read_line.read_line ~history ()
    inherit [Zed_utf8.t] LTerm_read_line.term term

    method show_box = false

    initializer self#set_prompt (React.S.const (make_prompt state))
  end
end
