open Core_kernel.Std
open Bap.Std
open Unsigned
open Options
open Format

class memory_map image options : Bil.storage = object(self : 's)
  val storage = Bitvector.Map.empty

  method private detect_write key data =
    let addr = Word.to_int key |> ok_exn in
    if addr >= 0x8000 && addr < 0xa000 then
      if options.di then printf "write to vram: %a\n" Word.pp data

  method save key data =
    {< storage = Map.add storage ~key ~data >}

  method private try_resolve_load_from_image key =
    let position = Word.to_int key |> ok_exn in
    let w8 = Word.of_int ~width:8 in
    match Z80_image.get_bytes image ~position ~size:1 with
    | [|v|] -> Some (w8 (UInt8.to_int v))
    | [||]-> None
    | _ -> failwith "1 byte requested, more than 1 returned."

  method load key : word option =
    match Map.find storage key with
    | Some v -> Some v
    | None -> self#try_resolve_load_from_image key
end

class memory_array image options : Bil.storage = object(self : 's)
  val storage = Array.create ~len:0x10000 None

  method save key data =
    let idx = Word.to_int key |> ok_exn in
    Array.set storage idx (Some data);
    {< storage = storage >}

  method private try_resolve_load_from_image idx =
    let w8 = Word.of_int ~width:8 in
    match Z80_image.get_bytes image ~position:idx ~size:1 with
    | [|v|] ->
      Array.set storage idx (Some (w8 (UInt8.to_int v)));
      Some (w8 (UInt8.to_int v))
    | [||]-> None
    | _ -> failwith "1 byte requested, more than 1 returned."

  method load key : word option =
    let idx = Word.to_int key |> ok_exn in
    try match Array.get storage idx with
      | Some v -> Some v
      | None -> self#try_resolve_load_from_image idx
    with | _ -> None
end
