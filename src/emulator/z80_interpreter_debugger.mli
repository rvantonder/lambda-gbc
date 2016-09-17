open Bap.Std
open Debugger_types

(** What's new: breakpoint list *)
class context :
  Z80_image.t ->
  Options.t ->
  object('s)
    inherit Z80_interpreter.context

    method add_breakpoint : breakpoint -> 's
    method remove_breakpoint : breakpoint -> 's
    method print_breakpoints : unit
  end

type send_event_stream = (Request.t option -> unit)

class ['a] z80_interpreter_debugger :
  Z80_image.t ->
  Options.t ->
  send_event_stream ->
  object('s)
    constraint 'a = #context
    inherit ['a] Z80_interpreter.z80_interpreter

  end
