
(** A Request issued to the z80 interpreter, which is handles *)
module Request : sig
  type t = Sleep of unit Lwt.t
         | Bp of int
end

(** The type of commands that can be issued *)
(** A command received on input from user on repl *)
module Command : sig
  type t = Pause
         | Resume
         | Bp of int [@@deriving sexp]
end

(** Does the work of processing commands and updating state *)
module Command_interpreter : sig
  (** We can make state opaque. So we do. *)
  type state

  val create : (Request.t option -> unit) -> state

  val process : state -> string -> (state * string)
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
