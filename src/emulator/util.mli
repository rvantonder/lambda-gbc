open Bap.Std
open Gbc_segment
open Options

(** Shortcuts for creating words *)
module Util_word : sig
  (** Creates 8-bit word *)
  val w8 : int -> word
  (** Creates 16-bit word *)
  val w16 : int -> word
end

(** Shortcuts for BIL expressions *)
module Util_bil : sig

  (** Creates an 8-bit BIL int *)
  val i8  : int -> exp
  (** Creates an 16-bit BIL int *)
  val i16 : int -> exp

  (** Shortcut for BIL int 1-bit 1 *)
  val true_  : exp
  (** Shortcut for BIL int 1-bit 0 *)
  val false_ : exp

  (** Creates a BIL statement that updates the CPU memory variable mem for the
      emulator (16-bit addresses, 8-bit values.*)
  val store_ : addr:addr -> word -> stmt
end

val time : string -> 'a lazy_t -> Options.t -> 'a

val dump_storage : Bil.storage -> int -> int -> unit

val dump_segment : Bil.storage -> Gbc_segment.t -> unit

val test_bit : word -> int -> bool

val set_bit : word -> int -> word

val reset_bit : word -> int -> word
