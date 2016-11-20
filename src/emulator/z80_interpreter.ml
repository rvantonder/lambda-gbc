open Core_kernel.Std
open Unsigned
open Bap.Std
open Format
open Z80_disassembler.Hunk
open Options
open Lwt

module Lifter = Z80_lifter

module CPU = Z80_cpu.CPU

open Logging

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

let sync_if_needed ctxt bil_stmt =
  match bil_stmt with
  | Bil.Move (v,_) -> Stmt.eval (sync v) ctxt
  | _ -> ctxt

let get_pc ctxt =
  match ctxt#pc with
  | Bil.Imm w -> Word.to_int w |> ok_exn
  | _ -> failwith "PC is not Imm."

(** Add x to pc and return updated ctxt *)
let add_pc ctxt (x : int) =
  match ctxt#pc with
  | Bil.Imm w -> ctxt#with_pc (Bil.Imm (Word.(w+(i16 x))))
  | _ -> ctxt

(** Substitute pc with value *)
let sub_pc ctxt stmts =
  (object inherit Stmt.mapper
    method! map_var v =
      (*if CPU.pc = v then*) (* TODO *)
      if Var.name v = "PC" then
        match ctxt#pc with
        | Bil.Imm pc -> Bil.int pc
        | _ -> failwith "Cannot substitute pc, not a value"
      else
        Bil.var v
  end)#run stmts

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
       (* XXX Hard-code LY to pass scanline wait check.
          0x90 = 144, the last row *)
       (*| 0xFF44 -> Some (i8 0x90)*)
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

  (** Total number of clock cycles *)
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

  val interrupts_enabled = false

  method interrupts_enabled = interrupts_enabled

  method set_interrupts_enabled (f : bool) =
    {< interrupts_enabled = f >}

  method private value_to_word =
     function
     | Bil.Imm w -> Some w
     | _ -> None

  method read_reg reg =
    let to_value bil_result =
      Bil.Result.value bil_result |>
      self#value_to_word in
    Seq.find_exn self#bindings ~f:(fun (v,bil_result) ->
        Var.(v = reg)) |> snd |> to_value

  (** Special cases for incrementing cpu clock:
      (1) Conditional jump
      (2) Relative Conditional jump
      (3) Conditional call.
      These need more clocks if condition succeeds.
      We do the check here based on (a) ctxt state and
      (b) disassembled instruction pattern. We can't
      do it in eval_jmp because we don't know which
      of the three the jump comes from without looking
      at the hunk anyway *)
  method inc_cpu_clock =
    let is_true flag b = match self#read_reg flag with
      | Some w -> if w = b then true else false
      | _ -> false in
    let flag_is_true =
      function
      | `FNC -> is_true CPU.fc Word.b0
      | `FC ->  is_true CPU.fc Word.b1
      | `FNZ -> is_true CPU.fz Word.b0
      | `FZ ->  is_true CPU.fz Word.b1
      | _ -> failwith "Unhandled flag in inc_cpu_clock" in
    let add_clocks true_ false_ flag =
      match flag_is_true flag with
      | true -> true_
      | false -> false_ in
    match current_hunk.stmt with
    | (`JR,[#Z80_disasm.Cond.t as flag; _]) ->
      (* For `JR, default is 8 if false. Else, 12 if true. TODO:
         embed hardcoded 12 in hunk data structure *)
      let _false = current_hunk.cycles in
      let _true = 12 in
      let clocks = add_clocks _true _false flag in
      {< cpu_clock = cpu_clock + clocks >}
    | (`JP,[#Z80_disasm.Cond.t as flag; _]) ->
      (* For `JP, default is 12 if false. Else, 16 if true *)
      let _false = current_hunk.cycles in
      let _true = 16 in
      let clocks = add_clocks _true _false flag in
      {< cpu_clock = cpu_clock + clocks >}
    | (`CALL,[#Z80_disasm.Cond.t as flag; _]) ->
      (* For `CALL, default is 12 if false. Else, 24 if true *)
      let _false = current_hunk.cycles in
      let _true = 24 in
      let clocks = add_clocks _true _false flag in
      {< cpu_clock = cpu_clock + clocks >}
    | _ -> {< cpu_clock = cpu_clock + current_hunk.cycles >}

  method mem_at_addr (addr : addr) : word option =
    let open Option in
    self#lookup (Z80_env.mem) >>= fun result ->
    match Bil.Result.value result with
    | Bil.Mem storage -> storage#load addr
    | _ -> None

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
    let to_imm8 = function
      | Some w -> sprintf "0x%04x" (Word.to_int w |> ok_exn)
      | _ -> "" in
    let (!) reg =
      let name = Var.name reg in
      let value = self#read_reg reg |> to_imm8 in
      sprintf "%s=%s" name value in
    let to_imm1 = function
      | Some w -> if w = Word.b1 then "▣" else "☐"
      | _ -> "" in
    let (!!) flag =
      let name = Var.name flag in
      let value = self#read_reg flag |> to_imm1 in
      sprintf "%s=%s" name value in
    let print2 reg flag = printf "│%8s%4s%-11s│\n%!" !reg " " !!flag in
    printf "\n%!";
    printf "┌%s┐\n%!" "----------------------";
    print2 CPU.af CPU.fz;
    print2 CPU.bc CPU.fn;
    print2 CPU.de CPU.fh;
    print2 CPU.hl CPU.fc;
    printf "│%22s|\n%!" " ";
    printf "│%s %4s%8s|\n%!" (!CPU.sp) " " " ";
    printf "│PC=%s %4s%8s|\n%!" (to_imm8 (self#value_to_word self#pc)) " " " ";
    printf "|k=0x%04x %4s%8s|\n%!" self#k " " " ";
    printf "|clock=%04d %4s%8s|\n%!" (self#cpu_clock/4) " " " ";
    printf "└%s┘\n%!" "----------------------";


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

  val frame_steps = 69905

  method private frame_steps = frame_steps

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
  method! eval stmts = super#eval stmts

  method private wwrite_word addr w =
      let open Option in
      let open Z80_cpu.CPU in
      let store_ addr v = Bil.store ~mem:(Bil.var Z80_cpu.CPU.mem)
      ~addr:(Bil.int addr) (Bil.int v) LittleEndian `r8 in
      (* TODO use self#store directly *)
      let stmt = [Bil.(Z80_cpu.CPU.mem := store_ addr w)] in
      let open Monad.State in
      (* Does this do what I think it does? *)
      self#eval stmt >>= fun () ->
      get ()

  method private service_interrupt i =
    log_interrupt @@
    sprintf "Interrupt enabled. CPU servicing interrupt %d" i;
    get () >>= fun ctxt ->
    update (fun ctxt -> ctxt#set_interrupts_enabled false) >>= fun () ->
    match ctxt#mem_at_addr (Util.w16 0xFF0F) with
    | Some req ->
      let req = Util.reset_bit req i in
      self#wwrite_word (Util.w16 0xFF0F) req >>= fun ctxt ->
      let call_interrupt addr =
        let store_to16 ~(dst : exp) ~(src : exp) : stmt =
          let store_to =
            Bil.(store ~mem:(var Z80_env.mem)
                   ~addr:dst src LittleEndian `r16) in
          Bil.(Z80_env.mem := store_to) in
        let x = (Util.w8 0x0) in
            [Bil.(store_to16 ~dst:(var Z80_env.sp) ~src:(var Z80_env.pc));
             Bil.(Z80_env.sp := var Z80_env.sp - int (i16 2));
             Bil.(jmp (int Word.(x@.addr)))
            ] in
      (match i with
       | 0 -> self#eval (call_interrupt @@ Util.w8 0x40) >>= fun () -> get ()
       | 1 -> self#eval (call_interrupt @@ Util.w8 0x48) >>= fun () -> get ()
       | 2 -> self#eval (call_interrupt @@ Util.w8 0x50) >>= fun () -> get ()
       | 3 -> self#eval (call_interrupt @@ Util.w8 0x60) >>= fun () -> get ()
       | _ -> failwith "No such interrupt to service"
      )
    | None -> failwith "No req in service_interrupt"

  method private check_interrupts =
    log_interrupt "Checking interrupts";
    get () >>= fun ctxt ->
    match ctxt#interrupts_enabled with
    | true ->
      log_interrupt "Interrupts ENABLED";
      (match ctxt#mem_at_addr (Util.w16 0xFF0F) with
      | Some req ->
        if req > (Util.w8 0) then
          List.fold [0;1;2;3;4;5;6;7] ~init:(return ctxt)
            ~f:(fun acc (bit as i) ->
              match Util.test_bit req bit with
              | false -> acc
              | true ->
                log_interrupt @@ sprintf "Interrupt %d requested" i;
                acc >>= fun ctxt ->
                (match ctxt#mem_at_addr (Util.w16 0xFFFF) with
                | Some enabled ->
                   if Util.test_bit enabled bit then
                     self#service_interrupt i
                   else
                     acc
                 | None -> acc)
            )
        else
          return ctxt
      | None -> return ctxt)
    | false ->
      log_interrupt "Interrupts DISABLED";
      return ctxt

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
  method step_insn =
    get () >>= fun ctxt ->
    update (fun ctxt -> ctxt#decode) >>= fun () ->
    update (fun ctxt -> ctxt#lift) >>= fun () ->
    get () >>= fun ctxt ->
    match ctxt#get_current_bil with
    | [] -> put ctxt#advance
    | bil ->
      update (fun ctxt -> ctxt#advance) >>= fun () ->
      get () >>= fun ctxt ->
      sub_pc ctxt bil |>
      self#eval >>= fun () ->
      update (fun ctxt ->
          List.fold ~init:ctxt bil ~f:(fun ctxt bil ->
              sync_if_needed ctxt bil)) >>= fun () ->
      self#check_interrupts >>= fun ctxt ->
      update (fun ctxt -> ctxt#inc_k) >>= fun () ->
      update (fun ctxt -> ctxt#inc_cpu_clock)

  method step_frame =
    let rec repeat count =
      if count < self#frame_steps then
        self#step_insn >>= fun () ->
        get () >>= fun ctxt ->
        let cycles = ctxt#current_hunk.cycles in
        repeat (count+cycles)
      else
        return () in
    repeat 0

  (** 8. *)
  (** Unhandled instructions will simply advance pc. Need to store
      current statement execution in ctxt. ctxt should do lifting. *)
  method! eval_special s =
    get () >>= fun ctxt ->
    let open Z80_disassembler in
    print_top ();
    printf "\n%!";
    ctxt#print_cpu;
    print_bot ();
    printf "\n%!";
    printf "%a\n%!" Hunk.pp ctxt#current_hunk;
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

(*
let render options ctxt =
  let open Lwt in
  match ctxt#lookup (Z80_env.mem) with
  | Some result ->
    (match Bil.Result.value result with
     | Bil.Mem storage ->
       (*Z80_image.dump_vram storage*)
       if options.no_render then return ()
       else Screen.render storage
     | _ -> return ())
  | _ -> return ()
*)
