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

  method add_breakpoint bp = {< breakpoints = bp::breakpoints >}

  method remove_breakpoint bp =
    let breakpoints' = List.filter breakpoints ~f:((=) bp) in
    {< breakpoints = breakpoints' >}

  method print_breakpoints =
    List.iter self#breakpoints ~f:(fun bp ->
        printf "0x%x\n%!" bp)

end

type send_event_stream = (Request.t option -> unit)

class ['a] z80_interpreter_debugger image options send_stream = object(self)
  constraint 'a = #context
  inherit ['a] Z80_interpreter.z80_interpreter image options as super

  method! eval stmts =
    (*printf "EVALING\n%!";*)
    super#eval stmts >>= fun () ->
    get () >>= fun ctxt ->
    let pc = match ctxt#pc with
      | Bil.Imm w -> Word.to_int w |> ok_exn
      | _ -> failwith "PC undefined" in
    (*printf "\nPC is 0x%x\n%!" pc;*)
    if List.exists ctxt#breakpoints ~f:((=) pc) then
      (printf "BP triggered!\n%!";
       send_stream (Some Request.Pause));
    return ()

  (*  method! step_insn =
      get () >>= fun ctxt ->
      if (List.exists ctxt#breakpoints ~f:((=) ctxt#pc)) then
        return ()
      else
        super#step_insn*)

end
