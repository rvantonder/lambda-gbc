(** Mimics bap_disasm_arm_env.

    Except, arm_env uses vals to make construction easy of
    BAP types. We want to make construction easy by type Reg.
    But also BAP types, so not sure how to resolve this
*)

(** Create constructors for Z80 types *)
open Core_kernel.Std
open Bap.Std

module Z80 = Z80_disasm

(** Shortcut for creating regs *)
let (%:) name typ = Var.create name typ

let make_register reg typ =
  match reg with
  | #Z80.Reg8.t as reg -> Z80.Reg8.to_string reg %: typ
  | #Z80.Reg16.t as reg -> Z80.Reg16.to_string reg %: typ

(** Not super happy with this: A flag is a subset of cond: possibly
    negated. Bil should never lift a FNZ cond to a fnz flag register,
    for example. *)
let make_flag flag typ = Z80.Cond.to_string flag %: typ

let reg8 reg = make_register reg reg8_t
let reg16 reg = make_register reg reg16_t
let bool flag = make_flag flag bool_t

let a = reg8 `A
let b = reg8 `B
let c = reg8 `C
let d = reg8 `D
let e = reg8 `E
let f = reg8 `F
let h = reg8 `H
let l = reg8 `L
let i = reg8 `I
let r = reg8 `R
let af = reg16 `AF
let bc = reg16 `BC
let de = reg16 `DE
let hl = reg16 `HL
let pc = reg16 `PC
let sp = reg16 `SP
(* Not needed for gbc right now, but ok*)
let ix = reg16 `IX
let iy = reg16 `IY
let ixh = reg16 `IXH
let ixl = reg16 `IXL
let iyh = reg16 `IYH
let iyl = reg16 `IYL

let fz = bool `FZ
let fc = bool `FC
let fp = bool `FP
let fs = bool `FS
let fn = bool `FN
let fh = bool `FH

let var_of_gpr : [ Z80.Reg8.t | Z80.Reg16.t] -> var = function
  | `A -> a
  | `B -> b
  | `C -> c
  | `D -> d
  | `E -> e
  | `F -> f
  | `H -> h
  | `L -> l
  | `I -> i
  | `R -> r
  | `AF -> af
  | `BC -> bc
  | `DE -> de
  | `HL -> hl
  | `PC -> pc
  | `SP -> sp
  | `IX -> ix
  | `IY -> iy
  | `IXH -> ixh
  | `IXL -> ixl
  | `IYH -> iyh
  | `IYL -> iyl

(*let var_of_flag : Z80.Flag.t -> var = function
  | `FZ -> fz
  | `FC -> fc
  | `FP -> fp
  | `FS -> fs
  | `FN -> fn
  | `FH -> fh*)

(** Either a reg or flag, but not Op *)
let of_reg : [Z80.Reg8.t | Z80.Reg16.t ] -> var = function
  | #Z80.Reg8.t as reg -> var_of_gpr reg
  | #Z80.Reg16.t as reg -> var_of_gpr reg
(*  | #Z80.Flag.t as flag -> var_of_flag flag*)

let new_var name = Var.create name reg8_t

(** Unused, confusing, may get rid of it. Just want to match the bap
    interface for arm first *)
let mem = Var.create "mem" (mem32_t `r8)
