open React
open Core_kernel.Std
open Sexplib

open Debugger_types

(** A module for a daemon that interprets commands *)
module Command_interpreter = struct
  type state = {
    n : int;
    push_channel : (Request.t option -> unit)}

  let create push_channel =
    {n = 1; push_channel}

  let process history state (rq : string) =
    let open Option in
    let open Request in
    let parse rq = String.capitalize rq |> Sexp.of_string |> t_of_sexp in
    let state' =
      try match rq with
        (* An empty command will take the most recent command
           executed. XXX fix hd_exn to hd *)
        | "" -> LTerm_history.contents history |> List.hd_exn |> parse
                |> some |> state.push_channel; state
        | rq -> parse rq |> some |> state.push_channel; state
      with
      | Sexplib.Conv.Of_sexp_error (exn,sexp) -> state (*cmd could not be parsed *)
      | exn ->
        let s = Exn.to_string exn in
        printf "Uncaught exception in Debugger_command.ml: %s\n%!" s;
        state
    in
    (*let out = "evaluated " ^ rq in*)
    let out = "" in
    let new_state = { state' with n = state'.n + 1 } in
    (new_state, out)

end

(** Stolen repl loop from lambda-term/examples/repl.ml. No idea how
    it does the magic*)
module Repl = struct

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
