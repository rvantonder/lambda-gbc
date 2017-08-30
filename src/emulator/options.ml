open Format
open Fieldslib

type t = { v : bool
         ; di : bool
         ; no_render : bool
         ; filename : string
         ; hex_dump : bool
         ; disas : int option
         ; k : int option
         ; bootrom : bool
         ; frame_speed : float
         }
[@@deriving fields]

let print opt =
  let print_s_b x = printf "%s : %b" (Field.name x) (Field.get x opt) in
  Fields.iter
    ~v:print_s_b
    ~di:print_s_b
    ~bootrom:print_s_b
    ~no_render:print_s_b
    ~hex_dump:print_s_b
    ~filename:(fun f -> printf "%s : %s" (Field.name f) (Field.get f opt))
    ~disas:(fun dd ->
        match Field.get dd opt with
        | Some i -> printf "%s : 0x%x" (Field.name dd) i
        | None -> printf "%s : false" (Field.name dd))
    ~k:(fun k ->
        match Field.get k opt with
        | Some i -> printf "%s : %d" (Field.name k) i
        | None -> printf "%s : false" (Field.name k))
    ~frame_speed:(fun speed ->
        printf "%s : %f" (Field.name speed) (Field.get speed opt))
