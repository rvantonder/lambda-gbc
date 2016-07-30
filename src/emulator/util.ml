open Unix
open Lazy
open Core_kernel.Std
open Bap.Std
open Format
open Gbc_segment
open Options

let time tag f options =
  let t = Unix.gettimeofday () in
  let res = Lazy.force f in
  if options.v then
    Format.printf "[%s] Execution time: %f seconds\n%!" tag
      (Unix.gettimeofday () -. t);
  res

let dump_storage storage s e =
  List.iter (List.range s e) ~f:(fun addr_int ->
      let addr = Addr.of_int ~width:16 addr_int in
      if addr_int % 32 = 0 then printf "\n";
      match storage#load addr with
      | Some word ->
        printf "%02x " @@ (Word.to_int word |> ok_exn)
      (*printf "\t\t%a -> %a\n" Addr.pp addr Word.pp word*)
      | None -> printf "00 ");
  printf "\n"

let dump_segment storage (seg : Gbc_segment.t) =
  dump_storage storage seg.pos (seg.pos+seg.size)
