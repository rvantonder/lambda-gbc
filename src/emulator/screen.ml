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

let log_render s =
  let section = Lwt_log.Section.make "render" in
  Lwt_log.ign_debug_f ~section "%s" s


let verbose = false

let bit_pair_to_rgb bit1 bit2 =
  match bit1,bit2 with
  | 1,1 -> (255,255,255)
  | 0,1 -> (192,192,192)
  | 1,0 -> (96,96,96)
  | 0,0 -> (0,0,0)
  | _ -> failwith "impossible bit matching"

(**
   [byte0,byte1] : [(rgb1,rgb1,rgb1),...,(rgb8,rgb8,rgb8)]
*)
let byte_pair_to_rgb_list byte_pair =
  let mask = Word.one 8 in
  let rec aux i acc =
    if i = 8 then acc else
      match byte_pair with
      | byte1::byte2::[] ->
        let shiftw = Word.of_int ~width:8 i in
        let bit1 = Word.((byte1 lsr shiftw) land mask) in
        let bit2 = Word.((byte2 lsr shiftw) land mask) in
        let (!) = Fn.compose ok_exn Word.to_int in
        aux (i+1) ((bit_pair_to_rgb !bit1 !bit2)::acc)
      | _ -> failwith "only two elements expected" in
  aux 0 []

(**
   16 bytes to a tile.
   tile : 8x8

   2 bytes per row
   2 bits per pixel

   tile : 16 bytes list
*)
let tile_bytes_to_rgb tile =
  (*let rows = List.groupi tile ~break:(fun i _ _ -> i mod 2 = 0) |> List.rev in*)
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
  List.iteri row ~f:(fun i (r,g,b) ->
      match r,g,b with
      | 255,255,255 -> printf "z"
      | 192,192,192 -> printf "x"
      | 96,96,96 -> printf "v"
      | 0,0,0 -> printf "o"
      | _ -> ())

(** tile_row: one row of 32 tiles. Tile dimensions: 32 x 1.
    Pixel dimensions: 256 x 8.

    Take the i'th row from each of the tiles and stitch them together into
    one list of length 256. *)
let stitch_tile_row tile_row i =
  List.fold ~init:[] tile_row ~f:(fun acc l ->
      List.append acc (List.nth_exn l i))

(* 1024 tiles *)
let tiles_to_pixel_grid tiles =
  (* 32 x 32 tile grid.
     grid_rows has size 32 (one element = one row *)
  let grid_rows = List.groupi tiles ~break:(fun i _ _ -> i mod 32 = 0) in
  let rows' = List.concat_map grid_rows ~f:(fun row ->
      (* row: one row of 32 tiles which is 256 pixels in length, 8
         pixels thick *)
      (* stitch row for row and concatenate into one big list *)
      let rec aux i acc =
        if i = 8 then acc else
          aux (i+1) (acc @ (stitch_tile_row row i)) in
      aux 0 []) in
  (* we used concat_map and produced 65536 pixels. split at 256 *)
  let rows' = List.groupi rows' ~break:(fun i _ _ -> i mod 256 = 0) in
  rows'


(** Tiles: 1024 element list with 8x8 entries becomes 256x256 grid *)
let print_ascii_screen tiles =
  List.iteri tiles
    ~f:(fun i row -> print_flat_row row;
         printf "\n")

(** one byte to 16 byte lookup *)
let tiles_of_idxs storage base idxs : 'a list list Or_error.t =
  let open Or_error in
  let w16 = Addr.of_int ~width:16 16 in
  let (!) = Fn.compose ok_exn Addr.to_int in
  List.fold idxs ~init:(return []) ~f:(fun acc idx ->
      (* ugly af cast from 8 to 16: *)
      let idx = Addr.to_int idx |> ok_exn |> Addr.of_int ~width:16 in
      (* TODO keep word*)
      let range =
        List.range !Addr.(base+(idx*w16)) !Addr.(base+(idx*w16)+w16) in
      let tile : 'a list Or_error.t =
        List.fold range ~init:(return []) ~f:(fun inner_acc addr_int ->
            let addr = Addr.of_int ~width:16 addr_int in
            match storage#load addr with
            | Some word ->
              inner_acc >>= fun inner_acc ->
              return (word::inner_acc)
            | None ->
              log_render @@
              sprintf "Warning tiles_of_idxs : word 0x%04x does \
                       not exist in memory" addr_int;
              Or_error.error_string "Could not read memory. Bailing")
      in
      tile >>= fun tile ->
      acc >>= fun acc ->
      return ((List.rev tile)::acc))

(* Based on lcdc, offset is 0x1c00 or 0x1800 *)
(*
   Bit 3 - BG Tile Map Display Select
   0: $9800-$9BFF -> Map 0
   1: $9C00-$9FFF -> Map 1
*)

let get_tiles storage =
  let open Option in
  storage >>= fun storage ->
  let open Gbc_segment in
  storage#load (Addr.of_int ~width:16 0xFF40) >>= fun lcdc ->
  let mask = Word.of_int ~width:8 0x8 in
  (if Word.(lcdc land mask = (zero 8)) then
     (log_render "selecting vram tile MAP 0";
      "vram-tile-map-0")
   else (
     log_render "selecting vram tile MAP 1";
     "vram-tile-map-1")
  ) |> fun map_display_select ->
  let mask = Word.of_int ~width:8 0x10 in
  (if Word.(lcdc land mask = (zero 8)) then
     (log_render "selecting vram tile SET 0";
      "vram-tile-set-0") (*-127 - 128*)
   else (log_render "selecting vram tile SET 1";
         "vram-tile-set-1")) |> fun data_display_select -> (* 0 - 255 *)


  if verbose then
    Util.dump_segment storage (Gbc_segment.segment_of_name map_display_select);
  if verbose then
    Util.dump_segment storage (Gbc_segment.segment_of_name
                                 data_display_select);
  let map_segment = Gbc_segment.segment_of_name map_display_select in
  let data_display_segment = Gbc_segment.segment_of_name data_display_select in
  let range = List.range map_segment.pos (map_segment.pos
                                          +map_segment.size) in

  (* Nasty fucking vram hack just reading first addr *)
  let ft = List.nth_exn range 0 in
  let ft = Addr.of_int ~width:16 ft in
  log_render @@ sprintf "HACK: read MAP addr %a" Word.pps ft;
  match storage#load ft with
  | Some _ ->
    (** If a word does not exist in memory, bail and return None *)
    List.fold range ~init:(Some []) ~f:(fun acc addr_int ->
        let addr = Addr.of_int ~width:16 addr_int in
        acc >>= fun l ->
        storage#load addr >>= fun word -> Some (word::l))
    (* 1024 bytes *)
    >>= fun idxs ->
    (* tiles : 32 x 32
       tile 0    : 16 bytes
       tile 1    : 16 bytes
       ...
       tile 1024 : 16 bytes *)

    let base = Addr.of_int ~width:16 data_display_segment.pos in
    log_render @@ sprintf "HACK2: read DATA SET %a" Word.pps base;
    (match storage#load base with
     | Some _ ->
       let tiles = tiles_of_idxs storage base idxs in
       (* tiles' :
          [
             tile 1:      [
                          row 0 : [(rgb1,rgb1,rgb1);...;(rgb8,rgb8,rgb8)];
                          ...
                          row 8 : [(rgb56,rgb56,rgb56);...;(rgb64,rgb64,rgb64];
                          ]
             tile 2:      [
                          row 0 : ...
                          ...
                          row 8 : ...
                          ]

             tile ...

             tile 1024:   [
                          row 0 : ...
                          ...
                          row 8 : ...
                          ]
          ]
       *)
       (match tiles with
        | Ok tiles ->
          let tiles' = List.map tiles ~f:tile_bytes_to_rgb in
          log_render @@ "Firing tiles_to_pixel_grid";
          let tiles' = tiles_to_pixel_grid tiles' in
          (*print_ascii_screen tiles';*)
          return tiles'
        | Error _ ->
          log_render "VRAM does not contain values for 1024 tiles.";
          None)
     | None ->
       log_render "Fuck this, no VRAM SET hack.";
       None )
  | None ->
    log_render "Fuck this, no VRAM MAP hack.";
    None



(*    log_render @@
      sprintf "VRAM does not contain values for 1024 tiles. Found %d"
      (List.length idxs);
      None*)

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
*)

(*
let get_tiles' storage =
  match storage with
  | Some storage ->
    let open Gbc_segment in
    let lcdc = storage#load (Addr.of_int ~width:16 0xFF40) in
    let map_display_select =
      match lcdc with
      | Some w ->
        let mask = Word.of_int ~width:8 0x8 in (* Bit 3: 00001000 *)
        if Word.(w land mask = (zero 8)) then
          (log_render "selecting vram tile MAP 0";
           "vram-tile-map-0")
        else
          (log_render "selecting vram tile MAP 1";
           "vram-tile-map-1")
      | None -> log_render "Can't render map: no LCDC. \
                            Choosing vram tile map 0"; "vram-tile-map-0" in
    let data_display_select =
      match lcdc with
      | Some w -> printf "LCDC: %a\n" Word.pp w;
        let mask = Word.of_int ~width:8 0x10 in (* Bit 4: 00010000 *)
        if Word.(w land mask = (zero 8)) then
          (log_render "selecting vram tile SET 0";
           "vram-tile-set-0" (*-127 - 128*))
        else (
          log_render "selecting vram tile SET 1";
          "vram-tile-set-1") (* 0 - 255 *)
      | None -> log_render "Can't render set: no LCDC. \
                           Choosing tile set 0";
        "vram-tile-set-0" (* WARN *)
    in
    log_render @@ sprintf "map_display_select is: %s\n" map_display_select;
    log_render @@ sprintf "data_display_select is: %s\n" data_display_select;
    if verbose then
      Util.dump_segment storage (Gbc_segment.segment_of_name map_display_select);
    if verbose then
      Util.dump_segment storage (Gbc_segment.segment_of_name data_display_select);
    let map_segment = Gbc_segment.segment_of_name map_display_select in
    let data_display_segment = Gbc_segment.segment_of_name data_display_select in
    let range = List.range map_segment.pos (map_segment.pos+map_segment.size) in
    (* 1024 bytes *)
    let idxs = List.fold range ~init:[] ~f:(fun acc addr_int ->
        let addr = Addr.of_int ~width:16 addr_int in
        match storage#load addr with
        | Some word -> word::acc
        | None ->
          log_render @@
            sprintf "Warning idxs in get_tiles': word 0x%04x does \
                     not exist in memory" addr_int;
          []) in
    (* tiles : 32 x 32
       tile 0    : 16 bytes
       tile 1    : 16 bytes
       ...
       tile 1024 : 16 bytes *)
    let base = Addr.of_int ~width:16 data_display_segment.pos in
    let tiles = tiles_of_idxs storage base idxs in
    (* tiles' :
       [
          tile 1:      [
                       row 0 : [(rgb1,rgb1,rgb1);...;(rgb8,rgb8,rgb8)];
                       ...
                       row 8 : [(rgb56,rgb56,rgb56);...;(rgb64,rgb64,rgb64];
                       ]
          tile 2:      [
                       row 0 : ...
                       ...
                       row 8 : ...
                       ]

          tile ...

          tile 1024:   [
                       row 0 : ...
                       ...
                       row 8 : ...
                       ]
       ]
    *)
    let tiles' = List.map tiles ~f:tile_bytes_to_rgb in
    (*print_ascii_screen tiles';*)
    let tiles' = tiles_to_pixel_grid tiles' in
    tiles'
  | None -> []
*)

(*
let render storage =
  Render.run_lwt (get_tiles' (Some storage))
*)
