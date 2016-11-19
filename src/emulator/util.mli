open Bap.Std
open Gbc_segment
open Options

val time : string -> 'a lazy_t -> Options.t -> 'a

val dump_storage : Bil.storage -> int -> int -> unit

val dump_segment : Bil.storage -> Gbc_segment.t -> unit

val test_bit : word -> int -> bool

val set_bit : word -> int -> word

val reset_bit : word -> int -> word

val w : int -> word
