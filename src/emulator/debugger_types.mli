type breakpoint = int [@@ deriving sexp]

module Request : sig
  type printable = Regs
                 | Insn
                 | Mem of int [@@ deriving sexp, variants]

  type steppable = Frame
                 | Insn [@@ deriving sexp, variants]

  type t = Pause
         | Resume
         | Help
         | Render
         | Bp of int
         | Step of steppable
         | Print of printable [@@deriving sexp, variants]

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end
