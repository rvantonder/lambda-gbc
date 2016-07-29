open Core_kernel.Std
open Bap.Std

(** CPU model based on bap vars *)

module CPU : sig
  include module type of Z80_env
  include CPU
  val flags : Var.Set.t (* my own addition *)
end
