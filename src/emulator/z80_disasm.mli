(** The real reason why we have z80_disasm.ml* at all: to provide
    dedicated type signatures. later it will be useful for satisfying the
    BAP regular interface *)

open Z80_types

module Reg8 : sig
  include module type of Z80_reg8
end

module Reg16 : sig
  include module type of Z80_reg16
end

module Cond : sig
  include module type of Z80_cond
end

module Op : sig
  include module type of Z80_op
end

module Insn : sig
  include module type of Z80_insn
end

module Stmt : sig
  include module type of Z80_stmt
end
