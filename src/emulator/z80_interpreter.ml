open Core_kernel.Std
open Unsigned
open Bap.Std
open Format
open Z80_disassembler.Hunk
open Options
open Lwt

module Lifter = Z80_lifter

module CPU = Z80_cpu.CPU

let verbose = false
let debug = false
let verbose_sync = false
let verbose_load = false

let normalize = String.filter ~f:(function
    | '\n' | '\t' -> false
    | _ -> true)

let i16 = Word.of_int ~width:16

(** Disassemble and decode the next hunk *)
let fetch_hunk image pc =
  let hunks = Z80_disassembler.linear image ~start:pc ~size:4 in
  match hunks with
  | [] -> failwith "Nothing to disassemble!"
  | [hunk] -> hunk
  | _ -> failwith "Interpreter invariant broken: \
                   received more then one hunk in disassembly."

let sync_r16 =
  Bil.([CPU.af := (var CPU.a) ^ (var CPU.f);
        CPU.bc := (var CPU.b) ^ (var CPU.c);
        CPU.de := (var CPU.d) ^ (var CPU.e);
        CPU.hl := (var CPU.h) ^ (var CPU.l)])

let sync_r8 =
  Bil.([CPU.a := Bil.extract ~hi:15 ~lo:8 (var CPU.af);
        CPU.f := Bil.extract ~hi:7 ~lo:0  (var CPU.af);
        CPU.b := Bil.extract ~hi:15 ~lo:8 (var CPU.bc);
        CPU.c := Bil.extract ~hi:7 ~lo:0  (var CPU.bc);
        CPU.d := Bil.extract ~hi:15 ~lo:8 (var CPU.de);
        CPU.e := Bil.extract ~hi:7 ~lo:0  (var CPU.de);
        CPU.h := Bil.extract ~hi:15 ~lo:8 (var CPU.hl);
        CPU.l := Bil.extract ~hi:7 ~lo:0  (var CPU.hl)])

(** If a write occurs to 8 bit reg, sync 16 bit regs, and vice versa *)
let sync reg =
  if verbose_sync then
    printf "Syncing %a\n%!" Var.pp reg;
  match Var.typ reg with
  | Type.Imm 8 ->
    if verbose_sync then
      (printf "Syncing 16:\n";
       printf "%a\n" Bil.pp (sync_r16));
    sync_r16
  | Type.Imm 16 ->
    if verbose_sync then
      (printf "Syncing 8:\n";
       printf "%a\n" Bil.pp (sync_r8));
    sync_r8
  | Type.Mem _ -> [] (* skip mem *)
  | Type.Imm 1 -> [] (* skip flag *)
  | _ -> failwith "Invalid register type: not 8 or 16 bits"

(** Set pc to addr *)
let set_pc ctxt (addr : int) =
  ctxt#with_pc (Bil.Imm (i16 addr))

let get_pc ctxt =
  match ctxt#pc with
  | Bil.Imm w -> Word.to_int w |> ok_exn
  | _ -> failwith "PC is not Imm."

(** Add x to pc and return updated ctxt *)
let add_pc ctxt (x : int) =
  match ctxt#pc with
  | Bil.Imm w -> ctxt#with_pc (Bil.Imm (Word.(w+(i16 x))))
  | _ -> ctxt

class memory image options : Bil.storage = object(self : 's)
  val storage = Bitvector.Map.empty

  method private try_resolve_load_from_image key =
    let position = Word.to_int key |> ok_exn in
    if options.di then
      printf "position: 0x%04x\n" position;
    let i8 = Word.of_int ~width:8 in
    match Z80_image.get_bytes image ~position ~size:1 with
    | [| |] ->
      (match position with
       | 0xff44 -> Some (i8 0x90) (* XXX Hard-code LY to pass scanline
                                     wait check. 0x90 = 144, the last row *)
       | _ -> None)
    | [|v|] ->
      Some (i8 (UInt8.to_int v))
    | _ -> failwith "1 byte requested, more than 1 returned."

  method private detect_write key data =
    let addr = Word.to_int key |> ok_exn in
    if addr >= 0x8000 && addr < 0xa000 then
      if options.di then
        printf "write to vram: %a\n" Word.pp data
      else
        ()

  method save key data =
    if options.di then
      printf "Saving %a -> %a\n%!" Word.pp key Word.pp data;
    self#detect_write key data;
    {< storage = Map.add storage ~key ~data >}

  method load key : word option =
    if verbose_load then
      printf "Loading %a\n%!" Word.pp key;
    match Map.find storage key with
    | Some v -> Some v
    | None -> self#try_resolve_load_from_image key
end

class context image options = object(self : 's)
  inherit Bili.context as super

  val current_hunk = Z80_disassembler.Hunk.empty ()

  val current_bil = []

  (** Number of instructions executed *)
  val k = 0

  (** Number of clock cycles *)
  val cpu_clock = 0

  method cpu_clock = cpu_clock

  method k = k

  method current_hunk = current_hunk

  (** Set of addrs for which we have memory contents in the
      interpreter storage *)
  val addrs = Bitvector.Set.empty

  (** getter *)
  method addrs = addrs

  method save_addr addr = {< addrs = Set.add addrs addr >}

  method inc_k = {< k = k + 1>}

  method inc_cpu_clock =
    {< cpu_clock = cpu_clock + current_hunk.cycles >}

  (** Note that lookup can also be done in interpreter, and it returns 'a r *)
  method dump_ram =
    match self#lookup (Z80_env.mem) with
    | Some result ->
      (match Bil.Result.value result with
       | Bil.Mem storage -> Set.iter self#addrs ~f:(fun addr ->
           match storage#load addr with
           | Some word ->
             if options.di then
               printf "\t%a -> %a\n" Addr.pp addr Word.pp word
           | None -> ())
       | _ -> ())
    | _ -> ()

  method dump_vram =
    printf "In DUM VRAM\n%!";
    match self#lookup (Z80_env.mem) with
    | Some result ->
      (match Bil.Result.value result with
       | Bil.Mem storage -> Set.iter self#addrs ~f:(fun addr ->
           let addr_int = Word.to_int addr |> ok_exn in
           match storage#load addr with
           | Some word when addr_int >= 0x8000 && addr_int < 0xa000 &&
                            (Word.to_int word |> ok_exn) <> 0 ->
             (*if options.di then*)
             printf "\t\t%a -> %a\n" Addr.pp addr Word.pp word
           | _ -> ())
       | _ -> ())
    | _ -> ()

  method print_cpu =
    let result_of_reg reg =
      Seq.find_exn self#bindings ~f:(fun (v,bil_result) ->
          Var.equal v reg) |> snd in
    let to_value bil_result = Bil.Result.value bil_result in
    let to_imm8 = function
      | Bil.Imm w -> sprintf "0x%04x" (Word.to_int w |> ok_exn)
      | _ -> "" in
    let (!) reg =
      let name = Var.name reg in
      let value = result_of_reg reg |> to_value |> to_imm8 in
      sprintf "%s=%s" name value in
    let to_imm1 = function
      | Bil.Imm w -> if w = Word.b1 then "▣" else "☐"
      | _ -> "" in
    let (!!) flag =
      let name = Var.name flag in
      let value = result_of_reg flag |> to_value |> to_imm1 in
      sprintf "%s=%s" name value in
    let print2 reg flag = printf "│%8s%4s%-11s│\n" !reg " " !!flag in
    print2 CPU.af CPU.fz;
    print2 CPU.bc CPU.fn;
    print2 CPU.de CPU.fh;
    print2 CPU.hl CPU.fc;
    printf "│%22s|\n" " ";
    printf "│%s %4s%8s|\n" (!CPU.sp) " " " ";
    printf "│PC=%s %4s%8s|\n" (to_imm8 self#pc) " " " ";
    printf "|k=0x%04x %4s%8s|\n" self#k " " " ";
    printf "|clock=0x%04x %4s%8s|\n" self#cpu_clock " " " "

  method print_lifted_stmts stmts =
    let purple = "\x1b[45m" in
    let restore = "\x1b[40m" in
    List.iter stmts ~f:(fun stmt ->
        Format.printf "%22s%s%s%s\n" " "
          purple (Bap.Std.Stmt.to_string stmt |> normalize) restore)

  method get_current_bil = current_bil

  (** Hunk has the information we need to know how much to advance by *)
  method advance =
    add_pc self (List.length current_hunk.bytes)

  (** Get PC, decode at current PC *)
  method decode =
    let open Z80_disassembler in
    let pc = get_pc self in
    if debug then
      printf "Current decode PC: 0x%04x\n" pc;
    let hunk = fetch_hunk image pc in
    if debug then
      printf "Current hunk %a\n" Hunk.pp hunk;
    {< current_hunk = hunk >}

  (** Substitute pc with value *)
  method sub_pc stmts =
    (object inherit Stmt.mapper
      method! map_var v =
        if Var.name v = "PC" then
          match self#pc with
          | Bil.Imm pc -> Bil.int pc
          | _ -> failwith "Cannot substitute pc, not a value"
        else
          Bil.var v
    end)#run stmts

  method lift =
    let bil = Lifter.lift current_hunk.stmt in
    if options.di then
      self#print_lifted_stmts bil;
    {< current_bil = bil >}

end

let print_top () =
  let s = List.init 22 ~f:(fun x -> "─")
          |> String.concat in
  printf "┌%s┐" s

let print_bot () =
  let s = List.init 22 ~f:(fun x -> "─")
          |> String.concat in
  printf "└%s┘" s

open Monad.State

(** Advancing is explicit, except for jmp *)
class ['a] z80_interpreter image options = object(self)
  constraint 'a = #context
  inherit ['a] bili as super

  method print_interpreted_stmts stmts =
    let aqua = "\x1b[46m" in
    let restore = "\x1b[40m" in
    List.iter stmts ~f:(fun stmt ->
        Format.printf "%22s%s%s%s\n" " "
          aqua (Bap.Std.Stmt.to_string stmt |> normalize) restore)

  (** Memory related operations (expi) *)
  method! empty = new memory image options

  (** When it seems Exp.Load in bil, it first performs
      expi#eval_load, which internally calls #load_word, which calls
      #load*)
  method! eval_load ~mem ~addr endian sz =
    if options.di then
      printf "Entered eval_load: %a %a el sz \n%!" Exp.pp mem Exp.pp addr;
    super#eval_load mem addr endian sz

  method! eval_store ~mem ~addr word endian sz =
    if options.di then
      printf "Entered eval_store\n%!";
    super#eval_store mem addr word endian sz

  method! store storage addr word =
    if options.di then
      printf "Entering store!\n%!";
    super#store storage addr word >>= fun r ->
    get () >>= fun ctxt ->
    put (ctxt#save_addr addr) >>= fun () ->
    return r

  method! load storage addr =
    if options.di then
      printf "Entering load!\n%!";
    super#load storage addr >>= fun r ->
    return r

  (** BILI base class methods *)
  (** 1. *)
  (** Advance should be called after each set of lifted statements
      corresponding to one hunk has been interpreted. Not when every move is
      interpreterd (eval_move), as I mistakenly did before.

      Also, FIRST advance PC, and THEN substitute PC (for jumps THEN
      eval. this is how conditional jumps for z80 also expect: use
      the updated PC address in the calculation, and then rewrite it
      to destination of jumps if needed.

      Subtlety: a jump condition can provide an empty list of next
      stmts to execute within the interpreter, bypassing our check in
      the run loop (so it can't be detected there. We must detect it
      here, and not advance pc in that case, it is already where it
      needs to be! *)
  method! eval stmts =
    match stmts with
    | [] -> super#eval stmts
    | stmts ->
      update (fun ctxt -> ctxt#advance) >>= fun _ ->
      get () >>= fun ctxt ->
      let stmts = ctxt#sub_pc stmts in
      if debug then
        self#print_interpreted_stmts stmts;
      super#eval stmts

  (** 2. *)
  method! eval_stmt stmt =
    super#eval_stmt stmt

  (** 3. *)
  method! eval_move v e =
    super#eval_move v e

  (** 4. *)
  method! eval_jmp (exp : Bil.exp) =
    super#eval_jmp exp

  (** 5. *)
  method! eval_while ~cond ~body =
    super#eval_while ~cond ~body

  (** 6. *)
  method! eval_if ~cond ~yes ~no =
    super#eval_if ~cond ~yes ~no

  (** 7. *)
  method! eval_cpuexn exn =
    super#eval_cpuexn exn

  (** 8. *)
  (** Unhandled instructions will simply advance pc. Need to store
      current statement execution in ctxt. ctxt should do lifting. *)
  method! eval_special s =
    get () >>= fun ctxt ->
    let open Z80_disassembler in
    print_top ();
    printf "\n";
    ctxt#print_cpu;
    print_bot ();
    printf "\n";
    printf "%a\n" Hunk.pp ctxt#current_hunk;
    failwith "Not implemented" |> ignore;
    get () >>= fun ctxt ->
    put ctxt#advance >>= fun () ->
    super#eval_special s
    (** /BILI base class methods *)
end

(** [ctxt] is synonymous with state. this prints the register state *)
let print_ctxt ctxt options =
  ctxt#bindings |> Seq.iter ~f:(fun (v,bil_result) ->
      let result = Bil.Result.value bil_result in
      match result with
      | Bil.Imm w ->
        if options.di then
          printf "Var: %a = %a\n" Var.pp v Word.pp w
      | Bil.Mem s -> () (* Our example doesn't use memory *)
      | Bil.Bot -> () (* Our example has no undefined results *))

(* TODO, I don't like how we create a new interpreter here and don't
   return it... shouldn't be needed... just return ctxt*)
let init image options =
  let stmts = Boot.clean_state in
  let ctxt = new context image options in
  let interpreter = new z80_interpreter image options in
  let start = interpreter#eval stmts in
  Monad.State.exec start ctxt

let init_default image options =
  let stmts = Boot.ready_state in
  let ctxt = new context image options in
  let interpreter = new z80_interpreter image options in
  let start = interpreter#eval stmts in
  Monad.State.exec start ctxt

let sync_if_needed ctxt bil_stmt =
  match bil_stmt with
  | Bil.Move (v,_) -> Stmt.eval (sync v) ctxt
  | _ -> ctxt



let render options ctxt =
  let open Lwt in
  match ctxt#lookup (Z80_env.mem) with
  | Some result ->
    (match Bil.Result.value result with
     | Bil.Mem storage ->
       (*Z80_image.dump_vram storage*)
       if options.no_render then return ()
       else Screen.render options storage
     | _ -> return ())
  | _ -> return ()

let step_insn options interpreter ctxt image =
  (* Decode and set current hunk *)
  let ctxt = ctxt#decode in
  (* Lift current hunk and set current bil *)
  let ctxt = ctxt#lift in
  (* Decide what to do next with current bil*)
  match ctxt#get_current_bil with
  | [] -> ctxt#advance
  | bil ->
    let ctxt = Monad.State.exec (interpreter#eval bil) ctxt in
    let ctxt = List.fold ~init:ctxt bil ~f:sync_if_needed in
    let ctxt = ctxt#inc_k in
    let ctxt = ctxt#inc_cpu_clock in
    if options.di then
      (
        print_top ();
        printf "\n";
        ctxt#print_cpu;
        print_bot ();
        printf "\n"
      );

    (match options.k with
     | Some k ->
       (*0xbb34 *)
       if ctxt#k = k then
         (
           if options.v then
             (printf "Dumping vram\n";
              ctxt#dump_vram);
           (*if options.v then
             (printf "Rendering\n";
              render options ctxt);*)
           failwith @@ sprintf "0x%x steps reached" k)
     | None -> ());
    ctxt (* TODO handle interrupts here *)

let draw_bg ctxt tiles =
  Background.from_tile_list tiles ctxt
  |> Background.render

let draw_gary ctxt =
  let open Sprites in
  let open Qsprite in
  let gary = Sprites.gary ~offsetx:1 ~offsety:1 ctxt in
  Qsprite.move gary 8 8;
  Qsprite.render gary

(** TODO: why if i raise exception here does it get ignored? *)
let draw ui matrix tiles =
  let open LTerm_geom in
  let size = LTerm_ui.size ui in
  let ctxt = LTerm_draw.context matrix size in
  (*Format.printf "Size: %s\n" @@ LTerm_geom.string_of_size size;
    Format.printf "%b %b" (size.rows < 289) (size.cols < 1430);
    (if size.rows < 289 || size.cols < 1430 then
     raise (Failure "I'm not going to continue drawing. Screen too small"));*)
  LTerm_draw.clear ctxt;
  draw_bg ctxt tiles;
  draw_gary ctxt

let max_steps = 69905

(** step instruction until 69905 clock cycles have been hit. that's
    one frame. Problem: clock cycles isn't a multiple of 69905, so
    this is a slight approximation. *)
let step_frame options interpreter ctxt image =
  let rec repeat count ctxt =
    if count < max_steps then
      let ctxt' = step_insn options interpreter ctxt image in
      let cycles = ctxt#current_hunk.cycles in
      repeat (count+cycles) ctxt'
    else
      ctxt in
  repeat 0 ctxt

let storage_of_context ctxt =
  match ctxt#lookup (Z80_env.mem) with
  | Some result ->
    (match Bil.Result.value result with
     | Bil.Mem storage ->
       Some storage
     | _ -> None)
  | _ -> None

let ref_tiles = ref [] (* XXX get rid of it later *)

let update_tiles_from_mem options ctxt =
  let storage = storage_of_context ctxt in
  let tiles = Screen.get_tiles options storage in
  (match tiles with
   | Some tiles ->
     (*Screen.print_ascii_screen tiles;*)
     ref_tiles := tiles;
   | None -> ())

let check_small_screen ui =
  let open LTerm_geom in
  let size = LTerm_ui.size ui in
  (if size.rows < 289 || size.cols < 1430 then (* XXX thumb suck *)
     raise (Failure "I'm not going to continue drawing. Screen too small"))

let event_loop refresh_frame options interpreter ctxt image =
  let open Lwt in
  let open LTerm_key in

  let stream, push = Lwt_stream.create () in
  let sleepy,wakey = Lwt.wait () in

  let rec input_loop term ui ctxt =
    LTerm.read_event term >>= function
    | LTerm_event.Key { code = Escape } ->
      return ()
    | LTerm_event.Key { code = Enter } ->
      Lwt_io.read_line Lwt_io.stdin >>= fun s ->
      Lwt_io.printf "Pushing command: %s\n" s >>= fun () ->
      push (Some s);
      (match s with
       | "resume" ->
         printf "WAKING\n%!";
         Lwt.wakeup wakey ()
       | _ -> ());
      input_loop term ui ctxt
    | _ -> input_loop term ui ctxt in

  let rec frame_loop term ui ctxt =
    printf "In loop\n%!";
    (** Steps a frame *)
    let ctxt' = step_frame options interpreter ctxt image in

    (** RENDER HERE *)
    (** Set tiles from memory *)
    (*printf "Dumping vram\n%!";
      ctxt#dump_vram;*)
    update_tiles_from_mem options ctxt;
    LTerm_ui.draw ui;

    (** Sleep, check input, and loop *)
    Lwt_unix.sleep refresh_frame >>= fun _ ->
    (*Lwt_io.read_line Lwt_io.stdin >>= fun str ->
      Lwt_io.printlf "You typed %S%!" str >>= fun () ->*)
    Lwt_io.printf "Checking stream\n%!" >>= fun () ->
    let elems = Lwt_stream.get_available stream in
    Lwt_io.printf "Size events: %d\n%!" (List.length elems) >>= fun () ->
    match elems with
    | ["pause"] ->
      Lwt_io.printf "PAUSING\n%!" >>= fun () ->
      sleepy >>= fun () ->
      frame_loop term ui ctxt'
    | _ ->
      frame_loop term ui ctxt' in

  Lwt_io.printl "Starting event_loop" >>= fun () ->
  Lazy.force LTerm.stdout >>= fun term ->
  update_tiles_from_mem options ctxt; (* TODO probably safe to remove *)
  LTerm_ui.create term (fun ui matrix -> draw ui matrix !ref_tiles) >>= fun ui ->
  (*check_small_screen ui;*) (* TODO turn on later *)
  Lwt.finalize (fun () -> Lwt.join
                   [frame_loop term ui ctxt;
                    input_loop term ui ctxt;]
               ) (fun () -> LTerm_ui.quit ui)

let run options ctxt image =
  let interpreter = new z80_interpreter image options in
  let ctxt' = set_pc ctxt 0 in
  try Lwt_main.run (event_loop options.frame_speed options interpreter ctxt' image)
  with
  | LTerm_draw.Out_of_bounds ->
    failwith "Rendering is ON, out of bounds!\n"
  | e -> let s = sprintf "%s" @@ Exn.to_string e in
    failwith s
