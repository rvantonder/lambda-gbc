open Bap.Std
open Options

type t

val get_tiles : Bil.storage option
  -> (int * int * int) list list option

val print_ascii_screen : (int * int * int) list list -> unit

val render : t -> Z80_interpreter_debugger.context -> unit

val create : LTerm.t -> t
