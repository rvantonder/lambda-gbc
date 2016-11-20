open Core_kernel.Std
open Bap.Std
open Debugger_types
open Z80_interpreter

open Monad.State

open Logging


(** register callbacks for hooks like 'breakpoint triggered'? *)
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
    List.iter self#breakpoints ~f:(fun bp ->
        printf "0x%x\n%!" bp)

end

type send_event_stream = (Request.t option -> unit)

class ['a] z80_interpreter_debugger image options send_stream =
  object(self)
    constraint 'a = #context
    inherit ['a] Z80_interpreter.z80_interpreter image options as super

    method! eval stmts =
      log_ev_cpu_dbg_eval @@ Bil.to_string stmts;
      super#eval stmts

    (** Must call super#step_insn first. Consider that you may have stopped on a
        breakpoint, then manually say 'step_insn'. it should always step
        then. *)
    method! step_insn =
      super#step_insn >>= fun () ->
      get () >>= fun ctxt ->
      let pc = match ctxt#pc with
        | Bil.Imm w -> Word.to_int w |> ok_exn
        | _ ->
          log_ev_cpu_dbg_pc_undef "PC undefined!";
          failwith "PC undefined!" in
      if (List.exists ctxt#breakpoints ~f:((=) pc)) then
        (log_ev_cpu_bp_trigger @@ sprintf "BP hit: 0x%x" pc;
         log_ev_cpu_rq_snd "Pause";
         send_stream (Some Request.Pause));
      return ()

    (** In debug mode, do not call super eval_special, which wil terminate and say
        'not implimented' *)
    method! eval_special s=
      send_stream (Some Request.Pause);
      return ()

  end
