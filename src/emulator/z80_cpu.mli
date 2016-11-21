open Core_kernel.Std
open Bap.Std

module Env = Z80_env

module CPU : sig
  include CPU                     (* BAP CPU signature *)
  include module type of Env      (* ENV *)
  val flags : Var.Set.t           (* My own addition *)
end
