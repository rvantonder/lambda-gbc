open Bap.Std
open Core_kernel.Std

module CPU = Z80_cpu.CPU

let (++) = Set.union
let i8 x = Bil.int (Word.of_int ~width:8 x)
let i16 x = Bil.int (Word.of_int ~width:16 x)
let false_ = Bil.int Word.b0
let true_ = Bil.int Word.b1
let store_ addr v = Bil.store ~mem:(Bil.var CPU.mem)
    ~addr:(i16 addr) (i8 v) LittleEndian `r8

(** All registers and flags set to 0. Memory undefined *)
let clean_state =
  Set.fold ~init:[] (CPU.gpr ++ CPU.flags) ~f:(fun stmts v ->
      if Var.typ v = reg8_t then
        Bil.(v := i8 0)::stmts
      else if Var.typ v = reg16_t then
        Bil.(v := i16 0)::stmts
      else if Var.typ v = bool_t then
        Bil.(v := false_)::stmts
      else
        failwith "reg is not of type reg8_t, reg16_t or flag. \
                  Cannot continue.")

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
    CPU.pc := i16 0xFFFE;
    CPU.sp := i16 0x0100;
    CPU.fz := true_;
    CPU.fn := false_;
    CPU.fh := true_;
    CPU.fc := true_;

    CPU.mem := store_ 0xFF05 0x00;
    CPU.mem := store_ 0xFF06 0x00;
    CPU.mem := store_ 0xFF07 0x00;
    CPU.mem := store_ 0xFF10 0x80;
    CPU.mem := store_ 0xFF11 0xBF;
    CPU.mem := store_ 0xFF12 0xF3;
    CPU.mem := store_ 0xFF14 0xBF;
    CPU.mem := store_ 0xFF16 0x3F;
    CPU.mem := store_ 0xFF17 0x00;
    CPU.mem := store_ 0xFF19 0xBF;
    CPU.mem := store_ 0xFF1A 0x7F;
    CPU.mem := store_ 0xFF1B 0xFF;
    CPU.mem := store_ 0xFF1C 0x9F;
    CPU.mem := store_ 0xFF1E 0xBF;
    CPU.mem := store_ 0xFF20 0xFF;
    CPU.mem := store_ 0xFF21 0x00;
    CPU.mem := store_ 0xFF22 0x00;
    CPU.mem := store_ 0xFF23 0xBF;
    CPU.mem := store_ 0xFF24 0x77;
    CPU.mem := store_ 0xFF25 0xF3;
    CPU.mem := store_ 0xFF26 0xF1;
    CPU.mem := store_ 0xFF40 0x91;
    CPU.mem := store_ 0xFF42 0x00;
    CPU.mem := store_ 0xFF43 0x00;
    CPU.mem := store_ 0xFF45 0x00;
    CPU.mem := store_ 0xFF47 0xFC;
    CPU.mem := store_ 0xFF48 0xFF;
    CPU.mem := store_ 0xFF49 0xFF;
    CPU.mem := store_ 0xFF4A 0x00;
    CPU.mem := store_ 0xFF4B 0x00;
    CPU.mem := store_ 0xFFFF 0x00;
  ]
