open Bap.Std
open Core_kernel.Std

open Util.Util_bil
open Util.Util_word

module CPU = Z80_cpu.CPU


let (++) = Set.union

(** All registers and flags set to 0. Memory undefined. *)
let clean_state =
  let open Bil in
  Set.fold ~init:[] (CPU.gpr ++ CPU.flags) ~f:(fun stmts v ->
      match Var.typ v with
      | Type.Imm 8  -> (v := i8 0)::stmts (* if Var.typ v = reg8_t then *)
      | Type.Imm 16 -> (v := i16 0)::stmts (* reg16_t *)
      | Type.Imm 1  -> (v := false_)::stmts (* bool_t *)
      | _ -> failwith "reg is not of type reg8_t, reg16_t or flag.")
  |> fun stmts -> stmts @ [
      (* LCD must be enabled *)
      store_ (w16 0xFF40) (w8 0x91);
      (* We read from LY in GPU using option types. must be initialized *)
      store_ (w16 0xFF44) (w8 0x00);
      (* We read LCD status in GPU. must be initialized *)
      store_ (w16 0xFF41) (w8 0x00);
      (* Coincidence flag for GPU must be initialized *)
      store_ (w16 0xFF45) (w8 0x00);
      (* Interrupt request register must be initialized (see interrupts *)
      store_ (w16 0xFF0F) (w8 0x00);
    ]

let ready_state =
  let open Bil in
  [
    CPU.a := i8 0x01;
    CPU.f := i8 0xB0;
    CPU.b := i8 0x00;
    CPU.c := i8 0x13;
    CPU.d := i8 0x00;
    CPU.e := i8 0xD8;
    CPU.h := i8 0x01;
    CPU.af := i16 0x01B0;
    CPU.bc := i16 0x0013;
    CPU.de := i16 0x00D8;
    CPU.hl := i16 0x014D;
    CPU.sp := i16 0xFFFE;
    CPU.fz := true_;
    CPU.fn := false_;
    CPU.fh := true_;
    CPU.fc := true_;

    store_ (w16 0xFF05) (w8 0x00);
    store_ (w16 0xFF06) (w8 0x00);
    store_ (w16 0xFF07) (w8 0x00);
    store_ (w16 0xFF10) (w8 0x80);
    store_ (w16 0xFF11) (w8 0xBF);
    store_ (w16 0xFF12) (w8 0xF3);
    store_ (w16 0xFF14) (w8 0xBF);
    store_ (w16 0xFF16) (w8 0x3F);
    store_ (w16 0xFF17) (w8 0x00);
    store_ (w16 0xFF19) (w8 0xBF);
    store_ (w16 0xFF1A) (w8 0x7F);
    store_ (w16 0xFF1B) (w8 0xFF);
    store_ (w16 0xFF1C) (w8 0x9F);
    store_ (w16 0xFF1E) (w8 0xBF);
    store_ (w16 0xFF20) (w8 0xFF);
    store_ (w16 0xFF21) (w8 0x00);
    store_ (w16 0xFF22) (w8 0x00);
    store_ (w16 0xFF23) (w8 0xBF);
    store_ (w16 0xFF24) (w8 0x77);
    store_ (w16 0xFF25) (w8 0xF3);
    store_ (w16 0xFF26) (w8 0xF1);
    (* lcd enabled *)
    store_ (w16 0xFF40) (w8 0x91);
    store_ (w16 0xFF42) (w8 0x00);
    store_ (w16 0xFF43) (w8 0x00);
    (* coincidence flag *)
    store_ (w16 0xFF45) (w8 0x00);
    store_ (w16 0xFF47) (w8 0xFC);
    store_ (w16 0xFF48) (w8 0xFF);
    store_ (w16 0xFF49) (w8 0xFF);
    store_ (w16 0xFF4A) (w8 0x00);
    store_ (w16 0xFF4B) (w8 0x00);
    store_ (w16 0xFFFF) (w8 0x00);
    (* extra: for lcd *)
    store_ (w16 0xFF44) (w8 0x00);
    (* extra: lcd status *)
    store_ (w16 0xFF41) (w8 0x00);
    (* extra: interrupt request register http://www.codeslinger.co.uk/pages/projects/gameboy/interupts.html*)
    store_ (w16 0xFF0F) (w8 0x00);
  ]
