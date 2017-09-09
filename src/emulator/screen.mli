open Core_kernel.Std
open Bap.Std
open Options

val get_tiles_new :
  Bil.storage
  -> (int * int * int) list list option

val render :
  LTerm_draw.point array array ->
  Bil.storage
  -> unit option
