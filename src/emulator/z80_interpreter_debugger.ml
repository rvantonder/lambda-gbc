open Core_kernel.Std
open Bap.Std
open Debugger_types
open Z80_interpreter

open Monad.State

open Logging

class context image options = object (self)
  inherit Z80_interpreter.context image options as super

  val breakpoints : breakpoint list = []

  method breakpoints = breakpoints

  method add_breakpoint bp =
    log_ev_cpu_add_bp @@ sprintf "BP add: 0x%x" bp;
    {< breakpoints = bp::breakpoints >}

  method remove_breakpoint bp =
    let breakpoints' = List.filter breakpoints ~f:((=) bp) in
    {< breakpoints = breakpoints' >}

  method print_breakpoints =
    List.iter self#breakpoints ~f:(printf "0x%x\n%!")
end

type send_event_stream = (Request.t option -> unit)

class ['a] z80_interpreter_debugger image options send_stream =
  object(self)
    constraint 'a = #context
    inherit ['a] Z80_interpreter.z80_interpreter image options as super

    method! eval stmts =
      super#eval stmts

    (** Must call super#step_insn first. Consider that you may have stopped on a
        breakpoint, then manually say 'step_insn'. it should always step
        then. *)
    method! step_insn =
      super#step_insn >>= fun () ->
      get () >>= fun ctxt ->
      return ()

    (** In debug mode, do not call super eval_special, which wil terminate and say
        'not implimented' *)
    method! eval_special s =
      send_stream (Some Request.Pause);
      return ()
  end
