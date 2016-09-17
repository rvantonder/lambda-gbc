open Bap.Std
open Bil.Result

class context :
  Z80_image.t ->
  Options.t ->
  object('s)
    inherit Bili.context

    method current_hunk : Z80_disassembler.Hunk.t
    method print_cpu : unit

    method addrs : Bitvector.Set.t
    method advance : 's
    method cpu_clock : int
    method decode : 's
    method dump_ram : unit
    method dump_vram : unit
    method get_current_bil : stmt list
    method inc_cpu_clock : 's
    method inc_k : 's
    method k : int
    method lift : 's
    method print_lifted_stmts : stmt list -> unit
    method save_addr : word -> 's
  end

val init : Z80_image.t -> Options.t -> int -> context
val init_default : Z80_image.t -> Options.t -> int -> context

class ['a] z80_interpreter :
  Z80_image.t ->
  Options.t ->
  object('s)
    constraint 'a = #context
    inherit ['a] bili

    method step_insn : 'a u
    method step_frame : 'a u

    method print_interpreted_stmts : stmt list -> unit
  end
