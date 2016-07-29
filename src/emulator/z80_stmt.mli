open Format
open Z80_types
open Z80_op

(** If we wrapped this inside a module inside disassemble, we wouldn't
    need to expose the concrete type *)
type t = z80_insn * z80_op list

val pp : Format.formatter -> t -> unit

val to_string : t -> string
