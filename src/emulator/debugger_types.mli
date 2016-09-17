type breakpoint = int [@@ deriving sexp]

module Request : sig
  type printable = Regs
                 | Insn [@@ deriving sexp, variants]

  type steppable = Frame
                 | Insn [@@ deriving sexp, variants]

  type t = Pause
         | Resume
         | Help
         | Bp of int
         | Step of steppable
         | Print of printable [@@deriving sexp, variants]

  val pp : Format.formatter -> t -> unit
end
