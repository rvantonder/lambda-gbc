open Core_kernel.Std
open Bap.Std
open Debugger_types
open Z80_interpreter

open Monad.State


(** register callbacks for hooks like 'breakpoint triggered'? *)
class context image options = object (self)
  inherit Z80_interpreter.context image options as super

  val breakpoints : breakpoint list = []

  method breakpoints = breakpoints

  method add_breakpoint bp =
    let section_bp = Lwt_log.Section.make "ev_int_add_bp" in
    Lwt_log.ign_debug_f ~section:section_bp "BP add: 0x%x" bp;
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
  let section_rq_snd = Lwt_log.Section.make "ev_int_rq_snd" in
  let section_bp =     Lwt_log.Section.make "ev_int_bp_trigger" in
  let section_eval =   Lwt_log.Section.make "ev_int_dbg_eval" in
  let section_pc_undef =   Lwt_log.Section.make "ev_int_dbg_pc_undef" in
  object(self)
    constraint 'a = #context
    inherit ['a] Z80_interpreter.z80_interpreter image options as super

    method! eval stmts =
      Lwt_log.ign_debug_f ~section:section_eval "%s" @@ Bil.to_string stmts;
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
          Lwt_log.ign_fatal ~section:section_pc_undef "PC undefined!";
          failwith "PC undefined!" in
      if (List.exists ctxt#breakpoints ~f:((=) pc)) then
        (Lwt_log.ign_debug_f ~section:section_bp "BP hit: 0x%x" pc;
         Lwt_log.ign_debug ~section:section_rq_snd "Pause";
         send_stream (Some Request.Pause));
      return ()

  end
