open Core_kernel.Std
open Sexplib

type breakpoint = int [@@ deriving sexp]

(** A Request issued to the z80 interpreter, which is handles *)
module Request = struct
  type printable = Regs
                 | Insn [@@ deriving sexp, variants]

  type steppable = Frame
                 | Insn [@@ deriving sexp, variants]

  type t = Pause
         | Resume
         | Help
         | Render
         | Bp of breakpoint
         | Step of steppable
         | Print of printable [@@deriving sexp, variants]

  (* TODO: needs to handle int/steppable types *)
  let pp ppf rq = Format.fprintf ppf "%s" @@ Sexp.to_string @@ sexp_of_t rq

  let to_string rq = Sexp.to_string @@ sexp_of_t rq
end
