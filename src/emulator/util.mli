open Bap.Std
open Gbc_segment
open Options.Options

val time : string -> 'a lazy_t -> Options.Options.t -> 'a

val dump_storage : Bil.storage -> int -> int -> unit

val dump_segment : Bil.storage -> Gbc_segment.t -> unit
