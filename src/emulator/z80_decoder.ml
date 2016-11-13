open Core_kernel.Std
open Bap.Std
open Unsigned

(** Ticks:
https://github.com/CTurt/Cinoop/blob/master/source/cb.c#L275
https://github.com/CTurt/Cinoop/blob/master/source/cpu.c#L308

IS DIFFERENT FROM

https://github.com/taisel/GameBoy-Online/blob/master/js/GameBoyCore.js#L3875

wtf?!?!

My version looks (mostly) right compared to pandocs. except for ld that takes 20 wtf.
ctrl-f "CPU Instruction Set"

http://bgb.bircd.org/pandocs.htm#cgbregisters
*)

let verbose = true

(** For rotation operations, the format differs:

    7 6 5 4 3 2 1 0
    i i x x x r e g
    | | ----
    | | ` decodes RLC, RRC, RL, RR, SLA, SRA, SWAP, SRL
    ` ` ignored (0 = ROT)
*)

let decode_rot code =
  match code lsr 3 with
  | 0 -> `RLC
  | 1 -> `RRC
  | 2 -> `RL
  | 3 -> `RR
  | 4 -> `SLA
  | 5 -> `SRA
  | 6 -> `SWAP
  | 7 -> `SRL
  | _ -> failwith "Cannot convert cb rot insn from opcode"

let cb_insn_from_code code =
  match code lsr 6 with
  (* There is no instruction called ROT in types. This is a variant
     introduced here so that we can do further processing *)
  | 0 -> `ROT
  | 1 -> `BIT
  | 2 -> `RES
  | 3 -> `SET
  | _ -> failwith "Cannot convert cb insn from opcode"

(** 7 6 5 4 3 2 1 0
    - - x x x - - -

    shift left 3 and 'and' with
    0 0 0 0 0 1 1 1
*)
let idx_from_code (code : int) = (code lsr 3) land 7

let reg_from_code code =
  let index = code land 7 in
  match index with
  | 0 -> `B
  | 1 -> `C
  | 2 -> `D
  | 3 -> `E
  | 4 -> `H
  | 5 -> `L
  | 6 -> `HL
  | 7 -> `A
  | _ -> failwith "Invalid index, cannot convert register from opcode"

(** Advance 2, since CB counts as 1 *)
let decode_extended bytes pos cc =
  let (!) x = `Imm (Word.of_int ~width:8 x) in
  let (~>) = cc in
  match bytes with
  | [code;_;_] ->
    let reg = reg_from_code code in
    (match cb_insn_from_code code with
     | `BIT as insn ->
       let idx = idx_from_code code in
       (match reg with
        | `HL -> ~> (insn,[!idx ; reg]) 2 12
        | _ -> ~> (insn,[!idx ; reg]) 2 8)
     | `RES | `SET as insn ->
       let idx = idx_from_code code in
       (match reg with
        | `HL -> ~> (insn,[!idx ; reg]) 2 16
        | _ -> ~> (insn,[!idx ; reg]) 2 8)
     | `ROT ->
       let insn = decode_rot code in
       (match reg with
        | `HL -> ~> (insn,[reg]) 2 16
        | _ -> ~> (insn,[reg]) 2 8))
  | _ -> ~> (`Undef,[]) 1 0

(** TIMINGS:

    pandocs and GB manual disagree on some:

    http://www.codeslinger.co.uk/pages/projects/gameboy/files/GB.pdf
    http://bgb.bircd.org/pandocs.htm

    e.g.

    0xC3 JP : pandocs says 16. GB Manual says 12.
    0xCD Call: pandocs says 24. GB Manual says 12.

    This other reference http://www.devrs.com/gb/files/opcodes.html says
         (corresponds with pandocs):
         JP : 16 Call : 24

    If this one corresponds to no$gmb, we will use it.
*)

(** I order by opcode number, rather than mnemonic, because I'm following
    GameBoyCore.js for correctness, rather than the http://www.z80.info/z80code.txt
    reference *)
(** TODO: use length of ops : NO, can't, look at conditoinal jump. two
    operands but need to also pass flags. can do length of ops not
    counting flags, maybe. *)

(** TODO: double check that operand order is significant! (LD n, A)
    versus (LD A, n) *)
let decode bytes pos cc =
  let (~>) = cc in
  (* This to save me from spending hours on renaming
     the below after deciding to stick to variant types. Will
     do that once things are more stable *)
  let a = `A in
  let b = `B in
  let c = `C in
  let d = `D in
  let e = `E in
  let h = `H in
  let l = `L in
  let af = `AF in
  let bc = `BC in
  let de = `DE in
  let hl = `HL in
  let sp = `SP in
  let (!) x = `Imm (Word.of_int ~width:8 x) in
  match List.map bytes ~f:UInt8.to_int with
  | [0x00;_;_;_] -> ~> (`NOP, []) 1 4
  | [0x01;x;y;_] -> ~> (`LD, [bc; !y; !x]) 3 12           (* LD BC, NN  *)
  | [0x02;_;_;_] -> ~> (`LD, [ b; c; a]) 1 8              (* LD (BC), A *)
  | [0x03;_;_;_] -> ~> (`INC,[ bc ]) 1 8                  (* BC + 1     *)
  | [0x04;_;_;_] -> ~> (`INC,[ b ]) 1 4
  | [0x05;_;_;_] -> ~> (`DEC,[ b ]) 1 4
  | [0x06;x;_;_] -> ~> (`LD, [ b; !x]) 2 8
  | [0x07;_;_;_] -> ~> (`RLCA, [ a ]) 1 4
  | [0x08;x;y;_] -> ~> (`LD, [ !y; !x; sp]) 3 20  (* Different for gameboy. *)
  | [0x09;_;_;_] -> ~> (`ADD, [hl; bc]) 1 8
  | [0x0a;_;_;_] -> ~> (`LD, [ a; bc]) 1 8
  | [0x0b;_;_;_] -> ~> (`DEC, [ bc ]) 1 8
  | [0x0c;_;_;_] -> ~> (`INC, [ c ]) 1 4
  | [0x0d;_;_;_] -> ~> (`DEC, [ c ]) 1 4
  | [0x0e;x;_;_] -> ~> (`LD, [c ; !x]) 2 8
  | [0x0f;_;_;_] -> ~> (`RRCA, [ a ]) 1 4
  | [0x10;_;_;_] -> ~> (`Undef, []) 1 4  (* Different for gameboy: STOP *)
  | [0x11;x;y;_] -> ~> (`LD, [ de; !y; !x ]) 3 12
  | [0x12;_;_;_] -> ~> (`LD, [ de; a ]) 1 8
  | [0x13;_;_;_] -> ~> (`INC, [ de ]) 1 8
  | [0x14;_;_;_] -> ~> (`INC, [ d ]) 1 4
  | [0x15;_;_;_] -> ~> (`DEC, [ d ]) 1 4
  | [0x16;x;_;_] -> ~> (`LD, [ d; !x ]) 2 8
  | [0x17;_;_;_] -> ~> (`RLA, [ a ]) 1 4
  | [0x18;x;_;_] -> ~> (`JR, [ !x]) 2 12 (* no pc needed*)
  | [0x19;_;_;_] -> ~> (`ADD, [ hl; de ]) 1 8
  | [0x1A;_;_;_] -> ~> (`LD, [ a; de ]) 1 8
  | [0x1B;_;_;_] -> ~> (`DEC, [ de ]) 1 8
  | [0x1C;_;_;_] -> ~> (`INC, [ e ]) 1 4
  | [0x1D;_;_;_] -> ~> (`DEC, [ e ]) 1 4
  | [0x1E;x;_;_] -> ~> (`LD, [ e; !x ]) 2 8
  | [0x1F;_;_;_] -> ~> (`RRA, [ a; ]) 1 4
  | [0x20;x;_;_] -> ~> (`JR, [`FNZ; !x]) 2 8 (* 8 if cc false, 12 if true*)
  | [0x21;x;y;_] -> ~> (`LD, [ hl; !y; !x]) 3 12
  | [0x22;x;y;_] -> ~> (`LD, [hl; !(1); a]) 1 8 (* special gbc LD (HLI),A *)
  | [0x23;_;_;_] -> ~> (`INC, [hl]) 1 8
  | [0x24;_;_;_] -> ~> (`INC, [hl]) 1 4
  | [0x25;_;_;_] -> ~> (`DEC, [h]) 1 4
  | [0x26;x;_;_] -> ~> (`LD, [h; !x]) 2 8
  | [0x27;_;_;_] -> ~> (`DAA, [a]) 1 4
  | [0x28;x;_;_] -> ~> (`JR, [`FZ; !x]) 2 8 (* 8 if cc false, 12 if true*)
  | [0x29;_;_;_] -> ~> (`ADD, [hl ; hl]) 2 8
  | [0x2A;_;_;_] -> ~> (`LD, [a; hl; !(1)]) 1 8 (* special gbc LD A, (HLI) *)
  | [0x2B;_;_;_] -> ~> (`DEC, [hl]) 1 8
  | [0x2C;_;_;_] -> ~> (`INC, [l]) 1 4
  | [0x2D;_;_;_] -> ~> (`DEC, [l]) 1 4
  | [0x2E;x;_;_] -> ~> (`LD, [l; !x]) 2 8
  | [0x2F;_;_;_] -> ~> (`CPL, [a]) 1 4
  | [0x30;x;_;_] -> ~> (`JR, [`FNC; !x]) 2 8 (* 8 if cc false, 12 if true*)
  | [0x31;x;y;_] -> ~> (`LD, [sp; !y; !x]) 3 12
  (* special gbc semantics: Save a at hl and decrement hl. I
     communicate this information by passing an immediate with this LD by
     which to change *)
  | [0x32;_;_;_] -> ~> (`LD, [hl; !(-1); a]) 1 8
  | [0x33;_;_;_] -> ~> (`INC, [sp]) 1 8
  | [0x34;_;_;_] -> ~> (`INC, [hl]) 1 12 (* This is INC (HL) not INC HL. What to do. *)
  | [0x35;_;_;_] -> ~> (`DEC, [hl]) 1 12
  | [0x36;x;_;_] -> ~> (`LD, [hl;!x]) 2 12
  | [0x37;x;_;_] -> ~> (`SCF, []) 1 4  (* CY = 1 *)
  | [0x38;x;_;_] -> ~> (`JR, [`FC; !x]) 2 8 (* 8 if cc false, 12 if true*) (* IF CY then PC+PC+x *)
  | [0x39;x;_;_] -> ~> (`ADD, [hl;sp]) 2 8
  | [0x3A;x;y;_] -> ~> (`LD, [a ; hl; !(-1)]) 1 8 (* special gbc LD A, (HLD) *)
  | [0x3B;_;_;_] -> ~> (`DEC, [sp]) 1 8
  | [0x3C;_;_;_] -> ~> (`INC, [a]) 1 4
  | [0x3D;_;_;_] -> ~> (`DEC, [a]) 1 4
  | [0x3E;x;_;_] -> ~> (`LD, [a;!x]) 2 8
  | [0x3F;_;_;_] -> ~> (`CCF, []) 1 4 (*CY=~CY*)
  | [0x40;_;_;_] -> ~> (`LD, [b;b]) 1 4
  | [0x41;_;_;_] -> ~> (`LD, [b;c]) 1 4
  | [0x42;_;_;_] -> ~> (`LD, [b;d]) 1 4
  | [0x43;_;_;_] -> ~> (`LD, [b;e]) 1 4
  | [0x44;_;_;_] -> ~> (`LD, [b;h]) 1 4
  | [0x45;_;_;_] -> ~> (`LD, [b;l]) 1 4
  | [0x46;_;_;_] -> ~> (`LD, [b;hl]) 1 8 (* LD B, (HL) *)
  | [0x47;_;_;_] -> ~> (`LD, [b;a]) 1 4
  | [0x48;_;_;_] -> ~> (`LD, [b;c]) 1 4
  | [0x49;_;_;_] -> ~> (`LD, [c;c]) 1 4
  | [0x4A;_;_;_] -> ~> (`LD, [c;d]) 1 4
  | [0x4B;_;_;_] -> ~> (`LD, [b;e]) 1 4
  | [0x4C;_;_;_] -> ~> (`LD, [c;h]) 1 4
  | [0x4D;_;_;_] -> ~> (`LD, [c;l]) 1 4
  | [0x4E;_;_;_] -> ~> (`LD, [c;hl]) 1 8
  | [0x4F;_;_;_] -> ~> (`LD, [c;a]) 1 4
  | [0x50;_;_;_] -> ~> (`LD, [d;b]) 1 4
  | [0x51;_;_;_] -> ~> (`LD, [d;c]) 1 4
  | [0x52;_;_;_] -> ~> (`LD, [d;d]) 1 4
  | [0x53;_;_;_] -> ~> (`LD, [d;e]) 1 4
  | [0x54;_;_;_] -> ~> (`LD, [d;h]) 1 4
  | [0x55;_;_;_] -> ~> (`LD, [d;l]) 1 4
  | [0x56;_;_;_] -> ~> (`LD, [d;hl]) 1 8
  | [0x57;_;_;_] -> ~> (`LD, [d;a]) 1 4
  | [0x58;_;_;_] -> ~> (`LD, [e;b]) 1 4
  | [0x59;_;_;_] -> ~> (`LD, [e;c]) 1 4
  | [0x5A;_;_;_] -> ~> (`LD, [e;d]) 1 4
  | [0x5B;_;_;_] -> ~> (`LD, [e;e]) 1 4
  | [0x5C;_;_;_] -> ~> (`LD, [e;h]) 1 4
  | [0x5D;_;_;_] -> ~> (`LD, [e;l]) 1 4
  | [0x5E;_;_;_] -> ~> (`LD, [e;hl]) 1 8
  | [0x5F;_;_;_] -> ~> (`LD, [e;a]) 1 4
  | [0x60;_;_;_] -> ~> (`LD, [h;b]) 1 4
  | [0x61;_;_;_] -> ~> (`LD, [h;c]) 1 4
  | [0x62;_;_;_] -> ~> (`LD, [h;d]) 1 4
  | [0x63;_;_;_] -> ~> (`LD, [h;e]) 1 4
  | [0x64;_;_;_] -> ~> (`LD, [h;h]) 1 4
  | [0x65;_;_;_] -> ~> (`LD, [h;l]) 1 4
  | [0x66;_;_;_] -> ~> (`LD, [h;hl]) 1 8
  | [0x67;_;_;_] -> ~> (`LD, [h;a]) 1 4
  | [0x68;_;_;_] -> ~> (`LD, [l;b]) 1 4
  | [0x69;_;_;_] -> ~> (`LD, [l;c]) 1 4
  | [0x6A;_;_;_] -> ~> (`LD, [l;d]) 1 4
  | [0x6B;_;_;_] -> ~> (`LD, [l;e]) 1 4
  | [0x6C;_;_;_] -> ~> (`LD, [l;h]) 1 4
  | [0x6D;_;_;_] -> ~> (`LD, [l;l]) 1 4
  | [0x6E;_;_;_] -> ~> (`LD, [l;hl]) 1 8
  | [0x6F;_;_;_] -> ~> (`LD, [l;a]) 1 4
  | [0x70;_;_;_] -> ~> (`LD, [hl;b]) 1 8
  | [0x71;_;_;_] -> ~> (`LD, [hl;c]) 1 8
  | [0x72;_;_;_] -> ~> (`LD, [hl;d]) 1 8
  | [0x73;_;_;_] -> ~> (`LD, [hl;e]) 1 8
  | [0x74;_;_;_] -> ~> (`LD, [hl;h]) 1 8
  | [0x75;_;_;_] -> ~> (`LD, [hl;l]) 1 8
  | [0x76;_;_;_] -> ~> (`HALT, []) 1 4
  | [0x77;_;_;_] -> ~> (`LD, [hl;a]) 1 8
  | [0x78;_;_;_] -> ~> (`LD, [a;b]) 1 4
  | [0x79;_;_;_] -> ~> (`LD, [a;c]) 1 4
  | [0x7A;_;_;_] -> ~> (`LD, [a;d]) 1 4
  | [0x7B;_;_;_] -> ~> (`LD, [a;e]) 1 4
  | [0x7C;_;_;_] -> ~> (`LD, [a;h]) 1 4
  | [0x7D;_;_;_] -> ~> (`LD, [a;l]) 1 4
  | [0x7E;_;_;_] -> ~> (`LD, [a;hl]) 1 8
  | [0x7F;_;_;_] -> ~> (`LD, [a;a]) 1 4
  | [0x80;_;_;_] -> ~> (`LD, [a;b]) 1 4
  | [0x81;_;_;_] -> ~> (`LD, [a;c]) 1 4
  | [0x82;_;_;_] -> ~> (`LD, [a;d]) 1 4
  | [0x83;_;_;_] -> ~> (`LD, [a;e]) 1 4
  | [0x84;_;_;_] -> ~> (`LD, [a;h]) 1 4
  | [0x85;_;_;_] -> ~> (`LD, [a;l]) 1 4
  | [0x86;_;_;_] -> ~> (`ADD, [a;hl]) 1 8
  | [0x87;_;_;_] -> ~> (`ADD, [a;a]) 1 4
  | [0x88;_;_;_] -> ~> (`ADC, [a;b]) 1 4
  | [0x89;_;_;_] -> ~> (`ADC, [a;c]) 1 4
  | [0x8A;_;_;_] -> ~> (`ADC, [a;d]) 1 4
  | [0x8B;_;_;_] -> ~> (`ADC, [a;e]) 1 4
  | [0x8C;_;_;_] -> ~> (`ADC, [a;h]) 1 4
  | [0x8D;_;_;_] -> ~> (`ADC, [a;l]) 1 4
  | [0x8E;_;_;_] -> ~> (`ADC, [a;hl]) 1 8
  | [0x8F;_;_;_] -> ~> (`ADC, [a;a]) 1 4 (* XXX a;a or a;hl? *)
  | [0x90;_;_;_] -> ~> (`SUB, [a;b]) 1 4
  | [0x91;_;_;_] -> ~> (`SUB, [a;c]) 1 4
  | [0x92;_;_;_] -> ~> (`SUB, [a;d]) 1 4
  | [0x93;_;_;_] -> ~> (`SUB, [a;e]) 1 4
  | [0x94;_;_;_] -> ~> (`SUB, [a;h]) 1 4
  | [0x95;_;_;_] -> ~> (`SUB, [a;l]) 1 4
  | [0x96;_;_;_] -> ~> (`SUB, [a;hl]) 1 8
  | [0x97;_;_;_] -> ~> (`SUB, [a;a]) 1 4
  | [0x98;_;_;_] -> ~> (`SBC, [a;b]) 1 4
  | [0x99;_;_;_] -> ~> (`SBC, [a;c]) 1 4
  | [0x9a;_;_;_] -> ~> (`SBC, [a;d]) 1 4
  | [0x9b;_;_;_] -> ~> (`SBC, [a;e]) 1 4
  | [0x9c;_;_;_] -> ~> (`SBC, [a;h]) 1 4
  | [0x9d;_;_;_] -> ~> (`SBC, [a;l]) 1 4
  | [0x9e;_;_;_] -> ~> (`SBC, [a;hl]) 1 8
  | [0x9f;_;_;_] -> ~> (`SBC, [a;a]) 1 4
  | [0xA0;_;_;_] -> ~> (`AND, [b]) 1 4
  | [0xA1;_;_;_] -> ~> (`AND, [c]) 1 4
  | [0xA2;_;_;_] -> ~> (`AND, [d]) 1 4
  | [0xA3;_;_;_] -> ~> (`AND, [e]) 1 4
  | [0xA4;_;_;_] -> ~> (`AND, [h]) 1 4
  | [0xA5;_;_;_] -> ~> (`AND, [l]) 1 4
  | [0xA6;_;_;_] -> ~> (`AND, [hl]) 1 8
  | [0xA7;_;_;_] -> ~> (`AND, [a]) 1 4
  | [0xA8;_;_;_] -> ~> (`XOR, [b]) 1 4
  | [0xA9;_;_;_] -> ~> (`XOR, [c]) 1 4
  | [0xAA;_;_;_] -> ~> (`XOR, [d]) 1 4
  | [0xAB;_;_;_] -> ~> (`XOR, [e]) 1 4
  | [0xAC;_;_;_] -> ~> (`XOR, [h]) 1 4
  | [0xAD;_;_;_] -> ~> (`XOR, [l]) 1 4
  | [0xAE;_;_;_] -> ~> (`XOR, [hl]) 1 8
  | [0xAF;_;_;_] -> ~> (`XOR, [a]) 1 4
  | [0xB0;_;_;_] -> ~> (`OR, [b]) 1 4
  | [0xB1;_;_;_] -> ~> (`OR, [c]) 1 4
  | [0xB2;_;_;_] -> ~> (`OR, [d]) 1 4
  | [0xB3;_;_;_] -> ~> (`OR, [e]) 1 4
  | [0xB4;_;_;_] -> ~> (`OR, [h]) 1 4
  | [0xB5;_;_;_] -> ~> (`OR, [l]) 1 4
  | [0xB6;_;_;_] -> ~> (`OR, [hl]) 1 8
  | [0xB7;_;_;_] -> ~> (`OR, [a]) 1 4
  | [0xB8;_;_;_] -> ~> (`CP, [b]) 1 4
  | [0xB9;_;_;_] -> ~> (`CP, [c]) 1 4
  | [0xBA;_;_;_] -> ~> (`CP, [d]) 1 4
  | [0xBB;_;_;_] -> ~> (`CP, [e]) 1 4
  | [0xBC;_;_;_] -> ~> (`CP, [h]) 1 4
  | [0xBD;_;_;_] -> ~> (`CP, [l]) 1 4
  | [0xBE;_;_;_] -> ~> (`CP, [hl]) 1 8
  | [0xBF;_;_;_] -> ~> (`CP, [a]) 1 4
  | [0xC0;_;_;_] -> ~> (`RET, [`FZ]) 1 8
  | [0xC1;_;_;_] -> ~> (`POP, [bc]) 1 12
  | [0xC2;x;y;_] -> ~> (`JP, [`FZ; !y; !x]) 3 12 (* 12 if cc false, 16 if true, http://www.devrs.com/gb/files/opcodes.html http://bgb.bircd.org/pandocs.htm *)
  | [0xC3;x;y;_] -> ~> (`JP, [!y; !x]) 3 16
  | [0xC4;x;y;_] -> ~> (`JP, [`FZ; !y; !x]) 3 12 (* 12 if cc false, 16 if true *)
  | [0xC5;_;_;_] -> ~> (`PUSH, [bc]) 1 16
  | [0xC6;x;_;_] -> ~> (`ADD, [bc; !x]) 2 8
  | [0xC7;_;_;_] -> ~> (`RST, [!0]) 1 16
  | [0xC8;_;_;_] -> ~> (`RET, [`FZ]) 1 8
  | [0xC9;_;_;_] -> ~> (`RET, []) 1 16
  | [0xCA;x;y;_] -> ~> (`JP, [`FZ; !y; !x]) 3 12 (* 12 if cc false, 16 if true *)
  (* secondary opcode set *)
  | 0xCB::tl -> decode_extended tl pos cc
  | [0xCC;x;y;_] -> ~> (`CALL, [`FZ; !y; !x]) 3 12 (* 12 if cc false, 24 if true *)
  | [0xCD;x;y;_] -> ~> (`CALL, [!y; !x]) 3 24
  | [0xCE;x;_;_] -> ~> (`ADC, [a; !x]) 2 8
  | [0xCF;_;_;_] -> ~> (`RST, [!0x8]) 1 16
  | [0xD0;_;_;_] -> ~> (`RET, [`FC]) 1 8
  | [0xD1;_;_;_] -> ~> (`POP, [de]) 1 12
  | [0xD2;x;y;_] -> ~> (`JP, [`FC; !y; !x]) 3 12 (* 12 if cc false, 16 if true *)
  | [0xD3;_;_;_] -> failwith "Illegal opcode 0xD3"
  | [0xD4;x;y;_] -> ~> (`CALL, [`FC; !y; !x]) 3 12 (* 12 if cc false, 24 if true *)
  | [0xD5;_;_;_] -> ~> (`PUSH, [de]) 1 16
  | [0xD6;x;_;_] -> ~> (`SUB, [a; !x]) 2 8
  | [0xD7;_;_;_] -> ~> (`RST, [!0x10]) 1 16
  | [0xD8;_;_;_] -> ~> (`RET, [`FC]) 1 8 (* RET FC, carry *)
  | [0xD9;_;_;_] -> ~> (`RETI, []) 1 16
  | [0xDA;x;y;_] -> ~> (`PUSH, [`FC; !y; !x]) 3 12 (* JP FC, carry *)
  | [0xDB;_;_;_] -> failwith "Illegal opcode 0xDB"
  | [0xDC;x;y;_] -> ~> (`CALL, [`FC]) 1 12  (* 12 if cc false, 24 if true *) (* CALL FC, carry *)
  | [0xDD;_;_;_] -> failwith
                      (sprintf "Illegal opcode 0xDD at 0x%x" pos)
  | [0xDE;x;_;_] -> ~> (`SBC, [a; !x]) 2 8
  | [0xDF;_;_;_] -> ~> (`RST, [!0x18]) 1 16
  | [0xE0;x;_;_] -> ~> (`LD, [!0x00; !0xff; !x; a]) 2 12 (* special, save A at (FF00+byte)*)
  | [0xE1;_;_;_] -> ~> (`POP, [hl]) 1 12
  | [0xE2;_;_;_] -> ~> (`LD, [!0x00; !0xff; c; a]) 1 8 (*special, save A at (FF00+C)*)
  | [0xE3;_;_;_] -> failwith "Illegal opcode 0xE3"
  | [0xE4;_;_;_] -> failwith "Illegal opcode 0xE4"
  | [0xE5;_;_;_] -> ~> (`PUSH, [hl]) 1 16
  | [0xE6;x;_;_] -> ~> (`AND, [a; !x]) 2 8
  | [0xE7;_;_;_] -> ~> (`RST, [!0x20]) 1 16
  | [0xE8;x;_;_] -> ~> (`ADD, [sp; !x]) 2 16
  | [0xE9;_;_;_] -> ~> (`JP, [hl]) 1 4
  | [0xEA;x;y;_] -> ~> (`LD, [!x; !y; a]) 3 16 (* special, LD (word), A *)
  | [0xEB;_;_;_] -> failwith "Illegal opcode 0xEB"
  | [0xEC;_;_;_] -> failwith "Illegal opcode 0xEC"
  | [0xED;_;_;_] -> failwith "Illegal opcode 0xED"
  | [0xEE;x;_;_] -> ~> (`XOR, [a;!x]) 2 8
  | [0xEF;_;_;_] -> ~> (`RST, [!0x28]) 1 16
  | [0xF0;x;_;_] -> ~> (`LD, [a; !0x00; !0xff; !x]) 2 12 (* special, (load): LD A, (FF00+byte)*)
  | [0xF1;_;_;_] -> ~> (`POP, [af]) 1 12
  | [0xF2;_;_;_] -> failwith "Illegal opcode 0xF2;contention with 0xE2"
  | [0xF3;_;_;_] -> ~> (`DI, []) 1 4
  | [0xF4;_;_;_] -> failwith "Illegal opcode 0xF4"
  | [0xF5;_;_;_] -> ~> (`PUSH, [af]) 1 16
  | [0xF6;x;_;_] -> ~> (`OR, [a;!x]) 2 8
  | [0xF7;_;_;_] -> ~> (`RST, [!0x30]) 1 16
  | [0xF8;x;_;_] -> ~> (`LDHL, [sp; !x]) 2 12
  | [0xF9;_;_;_] -> ~> (`LD, [sp; hl]) 1 8
  | [0xFA;x;y;_] -> ~> (`LD, [sp; hl; !y; !x]) 3 16
  | [0xFB;x;y;_] -> ~> (`EI, []) 1 4
  | [0xFC;_;_;_] -> failwith "Illegal opcode 0xFC"
  | [0xFD;_;_;_] -> failwith "Illegal opcode 0xFD"
  | [0xFE;x;_;_] -> ~> (`CP, [!x]) 2 8 (** a is not strictly necessary here *)
  | [0xFF;_;_;_] -> ~> (`RST, [!0x38]) 1 16
  | [x;_;_;_] ->
    if verbose then printf "Unhandled instruction <%x>\n" x;
    ~> (`Undef,[]) 1 0
  | _ -> ~> (`Undef,[]) 1 0
