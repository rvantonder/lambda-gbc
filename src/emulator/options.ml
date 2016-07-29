open Format
open Fieldslib

module Options  = struct
  type t = {    v : bool;
                di : bool;
                no_render : bool;
                filename : string;
                hex_dump : bool;
                disas : int option;
                k : int option;
           }
    [@@deriving fields]
end

let print opt =
  Options.Fields.iter
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
