open Core_kernel.Std
open Bap.Std
open Z80_types
open Z80_op
open Format

type t = z80_insn * z80_op list

let pp ppf stmt =
  let insn = fst stmt in
  let op_list = snd stmt in
  let sexp_printer = sexp_of_list (Z80_op.sexp_of_t) op_list in
  fprintf ppf "%a %a" Z80_insn.pp insn Sexp.pp sexp_printer

let pps stmt =
  asprintf "%a" pp stmt

(** normalize to radare output. bit hacky *)
let to_string stmt =
  let insn_str = Z80_insn.to_string (fst stmt) |> String.lowercase in
  let ops_strs = List.map (snd stmt) ~f:(fun op ->
      match (op : Z80_op.t) with
      | `Imm w -> Word.to_string w
      | x -> Sexp.to_string (sexp_of_t x))
                 |> String.concat ~sep:", " in
  sprintf "%s %s" insn_str ops_strs
