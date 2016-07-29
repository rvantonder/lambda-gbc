open Core_kernel.Std

type t = {
  name : string;
  pos : int;
  size : int
} [@@deriving fields, bin_io, sexp, compare]

let pp fmt t = Format.fprintf fmt "%s" @@ name t

(** Hardcoded list of segments for gameboy layout. Segments are
    contiguous. Sections will describe some more detail about
    cartridge, etc.*)
let segs =
  let sc = Fields.create in (* gbc seg create *)
  [sc ~name:"cartridge" ~pos:0 ~size:32768;
   sc ~name:"vram" ~pos:0x8000 ~size:8192;
   sc ~name:"vram-tile-set-1" ~pos:0x8000 ~size:2048;
   sc ~name:"vram-tile-set-0" ~pos:0x8800 ~size:2048; (* Note: 1024 overlap between sets*)
   sc ~name:"vram-tile-map-0" ~pos:0x9800 ~size:1024;
   sc ~name:"vram-tile-map-1" ~pos:0x9c00 ~size:1024;
   sc ~name:"switchable-ram-bank" ~pos:0xA000 ~size:8192;
   sc ~name:"internal-ram" ~pos:0xC000 ~size:8192;
   sc ~name:"internal-ram-copy" ~pos:0xE000 ~size:7680;
   sc ~name:"sprites-OAM" ~pos:0xFE00 ~size:160;
   sc ~name:"mmapped-IO" ~pos:0xFF00 ~size:128;
   sc ~name:"zero-page" ~pos:0xFF80 ~size:128
  ]

(* Nice Field example: Field.name Gbc_segment.Fields.pos would
   return the name of the record label as a string, so "pos" *)
let segment_of_name (seg_name : string) : t=
  List.find_exn segs ~f:(fun seg -> Field.get Fields.name seg = seg_name)
