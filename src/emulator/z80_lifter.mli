open Core_kernel.Std
open Bap.Std
open Z80_env

(** Takes a basic instruction and memory (Image) and returns a sequence
    of BIL statements *)
val lift : Z80.Stmt.t -> stmt list
