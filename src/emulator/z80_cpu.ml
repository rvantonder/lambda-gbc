open Core_kernel.Std
open Bap.Std
module Env = Z80_env

(** Satisfy BAP CPU type *)
module CPU = struct
  include Env

  (** BAP CPU mem interface included in Env *)
  (** BAP CPU sp interface included in Env *)

  (** BAP CPU interface does not require pc. However, we use a
      pc variable when lifting jump statements, which is dynamically
      substituted at runtime.*)

  (** BAP CPU gpr interface *)
  let gpr = Var.Set.of_list [
      a; b; c; d; e; f; h; l;
      i; r; af; bc; de; hl; pc; sp ]

  (** Flags. Not part of BAP CPU interface *)
  let flags = Var.Set.of_list [
      fz; (* zero *)
      fc; (* carry *)
      fp; (* parity *)
      fs; (* sign *)
      fn; (* subtract *)
      fh  (* half carry *)
    ]

  (** BAP CPU flag interface *)
  let zf = fz
  let cf = fc
  let nf = fn
  let vf = fz (* give it some flag, or it fails *)

  let is = Var.same

  (** BAP CPU predicate interface *)
  let is_reg r = Set.mem gpr (Var.base r)
  let is_flag reg = Set.mem flags (Var.base reg)

  let is_sp = is sp
  let is_bp _ = false
  let is_pc = is pc

  let is_zf = is fz
  let is_cf = is fc
  let is_nf = is fn
  let is_vf _ = false
  let is_mem = is mem
end
