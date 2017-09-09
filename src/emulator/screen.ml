(** There are two good representations for rendering:

    1) A 1024 element list of the 8x8 tiles, represented by a list of
    list containing a 3-element rgb tuple. Tiles are good for
    lambda-term and coordinate-schemes.

    2) A 256x256 list list containing 3-element rgb tuple for screen rendering.
    This is good for debugging and ascii representation, but probably not so
    useful later on.
*)

open Bap.Std
open Core_kernel.Std
open Format

open Lwt_react
open Lwt
open LTerm_geom
open LTerm_text
open LTerm_key

open Logging

let verbose = false

let white_screen_ : 'a list list option ref = ref None

let white_screen =
  match !white_screen_ with
  | Some x -> x
  | None ->
    let l = ref [] in
    let row = ref [] in

    for _ = 0 to 255 do
      row := (255, 255, 255)::!row
    done;

    for _ = 0 to 255 do
      l :=  !row::!l
    done;
    white_screen_ := Some !l;
    !l


let bit_pair_to_rgb bit1 bit2 =
  match bit1,bit2 with
  | 1,1 -> (255, 255, 255)
  | 0,1 -> (192, 192, 192)
  | 1,0 -> (96, 96, 96)
  | 0,0 -> (0, 0, 0)
  | _ -> failwith "Impossible bit matching"

(** [byte0,byte1] : [(rgb1,rgb1,rgb1),...,(rgb8,rgb8,rgb8)] *)
let byte_pair_to_rgb_list byte_pair =
  let mask = Word.one 8 in
  let rec aux i acc =
    if i = 8 then acc
    else match byte_pair with
      | byte1::byte2::[] ->
        let shiftw = Word.of_int ~width:8 i in
        let bit1 = Word.((byte1 lsr shiftw) land mask) in
        let bit2 = Word.((byte2 lsr shiftw) land mask) in
        let (!) = Fn.compose ok_exn Word.to_int in
        aux (i+1) ((bit_pair_to_rgb !bit1 !bit2)::acc)
      | _ -> failwith "only two elements expected" in
  aux 0 []

(**
   Takes a list of 16 bytes, which represent one tile.
   8x8 pixels = 64 pixels, 16 bytes.

   2 bytes per row, 8 rows. 16 bytes for 8 rows
   2 bits per pixel
   tile : 16 bytes list
*)
let tile_bytes_to_rgb tile =
  (**
     row 0 : [byte0;byte1]
     row 1 : [byte2;byte3]
     ...
     row 8 : [byte15;byte16]
  *)
  let rows = List.foldi ~init:[] tile ~f:(fun i acc byte ->
      match i % 2 with
      | 1 ->
        (match acc with
         | hd::tl -> (byte::hd)::tl
         | [] -> acc)
      | 0 -> [byte]::acc
      | _ -> failwith "not multiple of two") |> List.rev in
  (* Page 24, gameboy manual *)
  (**
     row 0 : [(rgb1,rgb1,rgb1), (rgb2,rgb2,rgb2) ... (rgb8,rgb8,rgb8)]
     row 1 : ...
     ...
     row 8 : ...
  *)
  let tile = List.map rows ~f:byte_pair_to_rgb_list in
  tile

let print_flat_row row =
  List.iteri row ~f:(fun i -> function
      | 255,255,255 -> printf "z"
      | 192,192,192 -> printf "x"
      | 96,96,96 -> printf "v"
      | 0,0,0 -> printf "o"
      | _ -> ())

(**
    tile_row: one row of 32 tiles. Tile dimensions: 32 x 1.
    Pixel dimensions: 256 x 8.

    Take the i'th row from each of the tiles and stitch them together into
    one list of length 256.
*)
let stitch_tile_row tile_row i =
  List.fold ~init:[] tile_row ~f:(fun acc l ->
      List.append acc (List.nth_exn l i))

(** a list of 1024 tiles come in *)
let tiles_to_pixel_grid tiles =
  (* 32 x 32 tile grid. grid_rows has size 32 (one element = one row *)
  let grid_rows = List.groupi tiles ~break:(fun i _ _ -> i mod 32 = 0) in
  let rows' = List.concat_map grid_rows ~f:(fun row ->
      (* row: one row of 32 tiles which is 256 pixels in length, 8 pixels
         thick.stitch row for row and concatenate into one big list *)
      let rec aux i acc =
        if i = 8 then acc
        else aux (i+1) (acc @ (stitch_tile_row row i))
      in
      aux 0 [])
  in
  (* we used concat_map and produced 65536 pixels. split at 256 *)
  List.groupi rows' ~break:(fun i _ _ -> i mod 256 = 0)

(** Tiles: 1024 element list with 8x8 entries becomes 256 x 256 grid *)
let print_ascii_screen tiles =
  List.iteri tiles ~f:(fun i row ->
      print_flat_row row;
      printf "\n")

(**
   http://www.codeslinger.co.uk/pages/projects/gameboy/graphics.html

   FF40 - LCDC - LCD Control (R/W)
   Bit 7 - LCD Display Enable             (0=Off, 1=On)
   Bit 6 - Window Tile Map Display Select (0=9800-9BFF, 1=9C00-9FFF)
   Bit 5 - Window Display Enable          (0=Off, 1=On)
   Bit 4 - BG & Window Tile Data Select   (0=8800-97FF, 1=8000-8FFF)
   Bit 3 - BG Tile Map Display Select     (0=9800-9BFF, 1=9C00-9FFF)
   Bit 2 - OBJ (Sprite) Size              (0=8x8, 1=8x16)
   Bit 1 - OBJ (Sprite) Display Enable    (0=Off, 1=On)
   Bit 0 - BG Display (for CGB see below) (0=Off, 1=On)

   Based on lcdc, offset is 0x1c00 or 0x1800
   Bit 3 - BG Tile Map Display Select
   0: $9800-$9BFF -> Map 0
   1: $9C00-$9FFF -> Map 1
*)
let get_tiles_new storage =
  let open Option in
  let cast16 = Addr.of_int ~width:16 in
  let open Gbc_segment in
  storage#load (Addr.of_int ~width:16 0xFF40) >>= fun lcdc ->
  let mask = Word.of_int ~width:8 0x8 in
  let map_display_select = match Word.(lcdc land mask = (zero 8)) with
    | true -> "vram-tile-map-0"
    | false -> "vram-tile-map-1"
  in
  let mask = Word.of_int ~width:8 0x10 in
  let data_display_select = match Word.(lcdc land mask = (zero 8)) with
    | true -> "vram-tile-set-0"  (* -127 - 128 *)
    | false -> "vram-tile-set-1" (* 0 - 255 *)
  in
  let map_segment = Gbc_segment.segment_of_name map_display_select in
  let data_display_segment =
    Gbc_segment.segment_of_name data_display_select in
  let base = data_display_segment.pos in
  let tiles = ref [] in
  let tile = ref [] in
  (* If a word does not exist in memory, bail and return None *)
  try
    for tile_addr = map_segment.pos to
        map_segment.pos + map_segment.size - 1 do
      (* 1024 tiles arranged as 32 x 32 tiles.
         Each tile is 8x8 pixels.
         Each tile is 16 bytes.
         8 x 32 = 256.
         tile 0    : 16 bytes
         tile 1    : 16 bytes
         ...
         tile 1024 : 16 bytes *)
      match storage#load (cast16 tile_addr) with
      | Some tile_addr ->
        let tile_addr = Addr.to_int tile_addr |> ok_exn in
        (* read each byte of 16 bytes for the tile. There are 16 bytes in a
           tile *)
        for i = base+(tile_addr*16) to (base+(tile_addr*16)+16)-1 do
          match storage#load (cast16 i) with
          | Some word -> tile := (!tile@[word])
          | None -> ()
        done;
        tiles := !tiles@[!tile];
        tile := []
      | None -> ()
    done;
    (* tiles' :
       [
          tile 1:      [
                       row 0 : [(rgb1,rgb1,rgb1);...;(rgb8,rgb8,rgb8)];
                       ...
                       row 8 : [(rgb56,rgb56,rgb56);...;(rgb64,rgb64,rgb64)];
                       ]
          tile 2:      [
                       row 0 : ...
                       ...
                       row 8 : ...
                       ]

          tile ...

          tile 1024: [ row 0 : ... ... row 8 : ... ]
       ] *)
    let tiles' = List.map !tiles ~f:tile_bytes_to_rgb in
    (* return 256 x 256 list list with rgb tuples *)
    let tiles' = tiles_to_pixel_grid tiles' in
    Some tiles'
  with | _ -> None


let render matrix storage =
  let scroll_offset addr =
    let addr = Addr.of_int ~width:16 addr in
    let value = storage#load addr in
    match value with
    | Some v -> Word.to_int v |> Or_error.ok_exn
    | _ -> 0 in
  let scroll_offset_y = scroll_offset 0xFF42 in
  let scroll_offset_x = scroll_offset 0xFF43 in
  log_clock @@
  Format.sprintf
    "x offset: %d; y offset: %d" scroll_offset_x scroll_offset_y;
  match get_tiles_new storage with
  | Some tiles ->
    for i = 0 to 143 do
      for j = 0 to 159 do
        let r,g,b =
          List.nth_exn tiles (i+scroll_offset_y)
          |> fun row -> List.nth_exn row (j+scroll_offset_x)
        in
        let point : LTerm_draw.point =
          matrix.(i).(j) in
        matrix.(i).(j) <-
          { point with
            LTerm_draw.background = LTerm_style.rgb r g b }
      done
    done;
    Some ()
  | None -> None
