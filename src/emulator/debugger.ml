open React
open Core_kernel.Std
open Sexplib

(** A Request issued to the z80 interpreter, which is handles *)
module Request = struct
  type t = Sleep of unit Lwt.t
         | Bp of int
end

(** A command received on input from user on repl *)
module Command = struct
  type t = Pause
         | Resume
         | Bp of int [@@deriving sexp]
end

(** A module for a daemon that interprets commands *)
module Command_interpreter = struct
  type state = {
    (* The command number *)
    n : int;
    (* The Lwt push stream *)
    push_channel : (Request.t option -> unit);
    (* Some if we can resume after pause by waking this thread*)
    resume_after_pause : unit Lwt.u option}

  let create push_channel =
    {n = 1;
     push_channel;
     resume_after_pause = None}

  (** How pause/resume works:
      When we see pause, create a sleep/wake thread pair. Send the sleep
      thread to the frame_loop. When we see resume, wake it up. Send a
      reference of the wake thread along with the input loop.
  *)
  let try_do (cmd : Command.t) state =
    let open Command in
    match cmd,state with
    | Pause, {resume_after_pause = None;_} ->
      printf "Valid pause command!\n%!";
      let sleepy,wakey = Lwt.wait () in
      printf "Pushing sleepy!\n%!";
      state.push_channel (Some (Request.Sleep sleepy));
      {state with resume_after_pause = Some wakey}
    | Resume,{resume_after_pause = Some wakey;_} ->
      printf "Valid resume command!\n%!";
      Lwt.wakeup wakey ();
      {state with resume_after_pause = None}
    | Bp pos,_ -> printf "Breakpoint set @ %d\n%!" pos;
      state.push_channel (Some (Request.Bp pos));
      state
    | _ -> state

  let process state (cmd : string) =
    let open Command in
    let state' =
      try
        String.capitalize cmd |> Sexp.of_string |> Command.t_of_sexp
        |> fun cmd -> try_do cmd state
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

(** Stolen repl loop from lambda-term/examples/repl.ml. No idea how
    it does the magic*)
module Repl = struct

  (** When using dot notation for accesing record fields, we can
      qualify the field by the module directly
      https://realworldocaml.org/v1/en/html/records.html. Only for
      record types. Not function. *)
  (* Create a prompt based on the current interpreter state *)
  let make_prompt state =
    let prompt = Printf.sprintf "In  [%d]: " state.Command_interpreter.n in
    LTerm_text.eval [ LTerm_text.(S prompt) ]

  (* Format the interpreter output for REPL display *)
  let make_output state out =
    let open Command_interpreter in (* just to demonstrate *)
    let open LTerm_text in
    let output = Printf.sprintf "Out [%d]: %s" state.n out in
    LTerm_text.eval [ LTerm_text.(S output) ]

  class read_line ~term ~history ~state = object(self)
    inherit LTerm_read_line.read_line ~history ()
    inherit [Zed_utf8.t] LTerm_read_line.term term

    method show_box = false

    initializer self#set_prompt (React.S.const (make_prompt state))
  end
end
