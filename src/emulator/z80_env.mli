open Bap.Std

(** Translation layer from disassembly to bap vars *)

module Z80 = Z80_disasm

val a : var
val b : var
val c : var
val d : var
val e : var
val f : var
val h : var
val l : var
val i : var
val r : var
val af : var
val bc : var
val de : var
val hl : var
val pc : var
val sp : var

(** These are flag states, not to be confused with conditions (in
    types) *)
val fz : var
val fc : var
val fp : var
val fs : var
val fn : var
val fh : var

val of_reg : [Z80.Reg8.t | Z80.Reg16.t ] -> var

val new_var : string -> var

val mem : var
