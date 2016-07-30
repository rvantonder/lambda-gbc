open Bap.Std
open Options

val render : Options.t -> Bil.storage -> unit Lwt.t

val get_tiles : Options.t -> Bil.storage option
  -> (int * int * int) list list option

val print_ascii_screen : (int * int * int) list list -> unit
