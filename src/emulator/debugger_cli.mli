open Debugger_types

(** Does the work of processing commands and updating state *)
module Command_interpreter : sig
  type state

  val create : (Request.t option -> unit) -> state

  val process : LTerm_history.t -> state -> string -> (state * string)
end

(** Reads input and spits out output *)
module Repl : sig
  val make_prompt : Command_interpreter.state -> LTerm_text.t
  val make_output : Command_interpreter.state -> string -> LTerm_text.t

  class read_line :
    term:LTerm.t ->
    history:LTerm_read_line.history ->
    state:Command_interpreter.state -> object('s)
      inherit LTerm_read_line.read_line
      inherit [Zed_utf8.t] LTerm_read_line.term

      method show_box : bool
    end
end
