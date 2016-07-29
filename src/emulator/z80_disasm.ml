(** Mimic bap_disasm_arm. This will make it easy to have Z80 use the
    BAP regular interface... later. *)

open Z80_types
open Z80_stmt

module Reg8 = struct
  include Z80_reg8
end

module Reg16 = struct
  include Z80_reg16
end

module Cond = struct
  include Z80_cond
end

module Op = struct
  include Z80_op
end

module Insn = struct
  include Z80_insn
end

module Stmt = struct
  include Z80_stmt
end
