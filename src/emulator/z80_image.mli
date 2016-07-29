open Unsigned
open Bap.Std
open Format
type t

(** create the entire memory image represented by a gameboy color
    instance (65k *)
val create : unit -> mem

val image_from_file : filename:string -> t

(** Return x bytes starting at [position] *)
val get_bytes : t -> position:int -> size:int -> UInt8.t array

val to_string : t -> string

val print_memory : mem -> unit

val fold : t -> init:'accum -> f:('accum -> UInt8.t -> 'accum) -> 'accum

val size : t -> int

val to_memory : string -> mem
