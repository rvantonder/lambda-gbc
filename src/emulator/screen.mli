open Bap.Std
open Options

val get_tiles_new : Bil.storage option
  -> (int * int * int) list list option

val print_ascii_screen : (int * int * int) list list -> unit
