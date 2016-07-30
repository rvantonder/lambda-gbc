open Format
open Fieldslib

type t = {    v : bool;
              di : bool;
              no_render : bool;
              filename : string;
              hex_dump : bool;
              disas : int option;
              k : int option;
              bootrom : bool;
              frame_speed : float
         }
  [@@deriving fields]

let print opt =
  Fields.iter
    ~v:(fun di -> printf "%s : %b" (Field.name di) (Field.get di opt))
    ~di:(fun di -> printf "%s : %b" (Field.name di) (Field.get di opt))
    ~filename:(fun f -> printf "%s : %s" (Field.name f) (Field.get f opt))
    ~no_render:(fun nr -> printf "%s : %b" (Field.name nr) (Field.get nr opt))
    ~hex_dump:(fun hd -> printf "%s : %b" (Field.name hd) (Field.get hd opt))
    ~disas:(fun dd ->
        match Field.get dd opt with
        | Some i ->
          printf "%s : 0x%x" (Field.name dd) i
        | None -> printf "%s : false" (Field.name dd))
    ~k:(fun k ->
        match Field.get k opt with
        | Some i ->
          printf "%s : %d" (Field.name k) i
        | None -> printf "%s : false" (Field.name k))
    ~bootrom:(fun b -> printf "%s: %b" (Field.name b) (Field.get b opt))
    ~frame_speed:(fun speed ->
        printf "%s : %f" (Field.name speed) (Field.get speed opt))
