open Core_kernel
open Lazy
open Format
open Bap.Std
open Options
open Lwt_log

open Util

module Cmdline = struct
  open Options
  open Cmdliner

  let process_args
      v_opt
      di_opt
      filename_opt
      no_render_opt
      hex_dump_opt
      disas_opt
      k_opt bootrom_opt
      frame_speed_opt =
    let (!) opt default = Option.value opt ~default in
    let filename_opt = !filename_opt "tests/boot/DMG_ROM_WITH_SCREEN.bin" in
    { v = v_opt
    ; di = di_opt
    ; filename = filename_opt
    ; no_render = no_render_opt
    ; hex_dump = hex_dump_opt
    ; disas = disas_opt
    ; k = k_opt
    ; bootrom = bootrom_opt
    ; frame_speed = frame_speed_opt
    }

  let v_opt : bool Term.t =
    let doc = "verbose info" in
    Arg.(value & flag & info ["v"; "verbose"] ~doc)

  let di_opt : bool Term.t =
    let doc = "debug interpreter" in
    Arg.(value & flag & info ["di"; "debug-interpreter"] ~doc)

  let no_render_opt : bool Term.t =
    let doc = "do not render to screen" in
    Arg.(value & flag & info ["nr"; "no-render"] ~doc)

  let hex_dump_opt : bool Term.t =
    let doc = "Display hex dump of the image" in
    Arg.(value & flag & info ["hd"; "hex-dump"] ~doc)

  let disas_opt : int option Term.t =
    let doc = "Disassemble starting at address" in
    Arg.(value & opt (some int) None & info ["disas"] ~doc)

  (** If filename is not specified, this, options holds None. Replace with
      string constant *)
  let filename_opt : string option Term.t =
    let doc = "ROM filename path" in
    Arg.(value & opt (some string) None & info ["f"; "filename"] ~doc)

  let k_opt : int option Term.t =
    let doc = "execute k steps" in
    Arg.(value & opt (some int) None & info ["k"] ~doc)

  let bootrom_opt : bool Term.t =
    let doc = "use bootrom" in
    Arg.(value & flag & info ["bootrom"] ~doc)

  let frame_speed_opt : float Term.t =
    let doc = "Speed to update frames, in seconds" in
    Arg.(value & opt float (1./.60.) & info ["s"; "speed"] ~doc)

  let info : Term.info =
    Term.info ~doc:"A pure functional GBC emulator" "Lambda-GBC"

  let parse argv =
    Term.eval
      ~argv
      begin Term.(
          pure process_args
          $v_opt
          $di_opt
          $filename_opt
          $no_render_opt
          $hex_dump_opt
          $disas_opt
          $k_opt
          $bootrom_opt
          $frame_speed_opt), info
      end |> function
    | `Ok opts -> opts
    | _ -> exit 1
end

let debug = true

let disassemble_linear image options =
  match options.disas with
  | None -> ()
  | Some start ->
    let disassembly = Z80_disassembler.linear image start 0x100 in
    if options.v then printf "Size: %d@." @@ List.length disassembly;
    printf "%a" Z80_disassembler.pp disassembly

let read_image options =
  let open Z80_image in
  let filename = options.filename in
  if options.v then Format.printf "Reading image...\n%!";
  let image =
    Util.time
      "[image_from_file]"
      (lazy (image_from_file ~filename))
      options
  in
  if options.hex_dump then
    Format.printf "%s\n" @@
    Util.time "[to_string]" (lazy (to_string image)) options;
  image

let () =
  let options = Cmdline.parse Sys.argv in
  let image = read_image options in
  if options.v then printf "Image size: %d\n%!" (Z80_image.size image);
  disassemble_linear image options;
  if options.v then printf "Initializing interpreter...\n";
  Runner.run options image |> ignore
