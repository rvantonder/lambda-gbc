open Bap.Std
open Bil.Result

(** Disassemble and decode the next hunk *)
val fetch_hunk : Z80_image.t -> int -> Hunk.t

class context : Z80_image.t -> Options.t -> object('s)
    inherit Bili.context

    method current_hunk : Hunk.t
    method current_bil : bil
    method print_cpu : unit

    method interrupts_enabled : bool
    method set_interrupts_enabled : bool -> 's

    (** addrs tracks the addrs we have values for in memory.
        This is so that we can economically print sparse memory *)
    method addrs : Bitvector.Set.t
    method advance : 's
    (* XXX : cpu_clock @ 2**32 is only big enough to count up to 16 minutes
       worth of execution: (2**32/(70221*60))/60 = 16.98. 64k int will get us
       ~138k years. *)
    method cpu_clock : int
    method decode : 's
    method mem_at_addr : addr -> word option
    (* Use with caution! This is only to increment scanline of GPU, for
       example. Everything else should go through the standard store/load
       api of interpreter which we can catch *)
    method read_reg : var -> word option
    method dump_ram : unit
    method dump_vram : unit
    method get_current_bil : stmt list
    method inc_cpu_clock : 's
    method inc_k : 's
    method k : int
    method lift : 's
    method print_lifted_stmts : stmt list -> unit
    method save_addr : word -> 's

    method load_next : 's
  end

(** The only way to advance the interpreter should be through step_insn and
    step_frame *)
class ['a] z80_interpreter : Z80_image.t -> Options.t -> object('s)
    constraint 'a = #context
    inherit ['a] bili

    method step_insn : 'a u
    method step_frame : 'a u

    method print_interpreted_stmts : stmt list -> unit
  end
