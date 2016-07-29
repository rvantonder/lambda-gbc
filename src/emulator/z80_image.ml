open Core_kernel.Std
open Bap.Std
open Unsigned
open Format

type t = UInt8.t array

(** read binary data from file *)
(** validate that it is larger than 0x4000? *)
let image_from_file ~filename =
  In_channel.read_all filename
  |> String.to_array
  |> Array.map ~f:(Fn.compose UInt8.of_int Char.to_int)

let fold image ~init ~f =
  Array.fold image ~init ~f

let get_bytes image ~position ~size =
  try
    let offsets = List.range position (position+size) |> List.to_array in
    Array.map offsets ~f:(fun offset -> Array.get image offset)
  with | _ -> [| |] (* out of bounds *)

let to_string image =
  Array.map image ~f:UInt8.to_int |>
  Array.foldi ~init:"" ~f:(fun i acc v ->
      let acc =
        if i mod 16 = 0 then acc^(Format.sprintf "\n") else acc in
      acc^(Format.sprintf "%04x " v))

let size image = Array.length image

let print_memory mem = printf "%a\n" Memory.pp mem

let to_memory filename : mem =
  match Memory.of_file LittleEndian (Addr.of_int ~width:16 0) filename with (* TO REPORT *)
  | Ok memory -> memory
  | Error e ->
    failwith (sprintf "Could not create memory: %s" @@ Error.to_string_hum e)

let create () =
  let data = Bigstring.create 65536 in
  let start = Addr.of_int ~width:16 0 in
  Memory.create LittleEndian start data |> ok_exn
