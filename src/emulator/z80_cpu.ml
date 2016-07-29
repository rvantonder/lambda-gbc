open Core_kernel.Std
open Bap.Std
module Env = Z80_env

module CPU = struct
  include Env

  (** mem is the general memory type used during lifting *)
  let mem = mem
  (** this pc is ignored in interpreter, it can be safely removed (but
      this breaks the type expected by CPU currently

      Correction: we now use pc for designating relative jumps.
      However, pc is substituted by the interpreter 'at runtime. This
      is not the perfect solution: ultimately, we would not want pc in
      here, and we would want jump addresses to be resolved
      automatically by the disassembler. But then it it is no longer
      disassembling/lifting, but optimizing. So, we stick with this
      convention.*)
  let pc = pc
  let sp = sp

  let gpr = Var.Set.of_list [
      a; b; c; d; e; f; h; l;
      i; r; af; bc; de; hl; pc; sp ]

  (** STATE of flags *)
  let flags = Var.Set.of_list [
      fz; (* zero *)
      fc; (* carry *)
      fp; (* parity *)
      fs; (* sign *)
      fn; (* subtract *)
      fh  (* half carry *)
    ]

  let zf = fz
  let cf = fc
  let nf = fz (*TODO*)
  let vf = fc (*TODO*)

  let addr_of_pc mem = Addr.(Memory.min_addr mem ++ 8)

  let is = Var.same

  let is_reg r = Set.mem gpr (Var.base r)
  let is_flag reg = Set.mem flags (Var.base reg)

  let is_sp = is sp
  let is_bp _ = false (*TODO / unused *)
  let is_pc = is pc

  let is_mem = is mem

  let is_zf = is fz
  let is_cf = is fc
  let is_nf = is nf (*TODO / unused *)
  let is_vf = is vf (*TODO / unused *)
end
