open Bap.Std
open Options

type t

val get_tiles : Bil.storage option
  -> (int * int * int) list list option

val print_ascii_screen : (int * int * int) list list -> unit

val render : t -> Z80_interpreter_debugger.context -> 'a Lwt_mvar.t -> unit Lwt.t

val create : 'a Lwt_mvar.t ->  LTerm.t -> t
