open Format

type t = {
  name : string;
  pos : int;
  size : int
} [@@deriving fields, bin_io, sexp, compare]

val pp : formatter -> t -> unit

val segment_of_name : string -> t
