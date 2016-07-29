(** Mimics bap_disasm_arm_types. Arm has two ops: Reg of reg and Imm
    of word. Should be fine for us, but I want to keep the flags
    too. *)

open Unsigned
open Core_kernel.Std
open Bap.Std

type mem = Memory.t

exception Lifting_failed of string

(** ~~~~~~~~~~~~ *)
(** ~~~ Reg ~~~~ *)
(** ~~~~~~~~~~~~ *)
module Z80_reg8 = struct
  (** General purpose registers *)
  type gpr = [
    | `A (* accumulator *)
    | `B (* 8 bit counter *)
    | `C (* interface with hardware ports *)
    | `D (* Used with `E *)
    | `E (* Used with `D *)
    | `F (* Flags *)
    | `H (* Used with `L *)
    | `L (* Used with `H *)
    | `I (* Interrupts *)
    | `R (* Refresh register *)
  ] [@@deriving sexp]

  type t = gpr [@@deriving sexp]

  let pp ppf reg =
    Format.fprintf ppf "%s" @@ Sexp.to_string @@ sexp_of_t reg

  let to_string reg =
    Format.asprintf "%a" pp reg

  let pps () reg =
    to_string reg
end

type z80_reg8 = Z80_reg8.t [@@deriving sexp]

module Z80_reg16 = struct
  (** General purpose registers *)
  type gpr = [
    | `IXH (* High, first byte of IX *)
    | `IXL (* Low, second byte of IX *)
    | `IYH (* High, first byte of IY *)
    | `IYL (* Low, second byte of IY *)
    | `AF (* Not really used *)
    | `BC (* 16 bit counter *)
    | `DE (* Memory address (Destination) *)
    | `HL (* General 16 bit register *)
    | `PC (* Program counter *)
    | `SP (* Stack pointer *)
    | `IX (* Index register, like HL, but slower *)
    | `IY (* Index register *)
  ] [@@deriving sexp]

  type t = gpr [@@deriving sexp]

  let pp ppf reg =
    Format.fprintf ppf "%s" @@ Sexp.to_string @@ sexp_of_t reg

  let to_string reg =
    Format.asprintf "%a" pp reg

  let pps () reg =
    to_string reg
end

type z80_reg16 = Z80_reg16.t [@@deriving sexp]

(** ~~~~~~~~~~~~ *)
(** ~~~ Cond ~~~ *)
(** ~~~~~~~~~~~~ *)
module Z80_cond = struct
  type cond = [
    | `FZ
    | `FNZ
    | `FC
    | `FNC
    (* not used currently, will update when needed...*)
    | `FP
    | `FS
    | `FN
    | `FH
  ] [@@deriving sexp]

  type t = cond [@@deriving sexp]

  let pp ppf cond = Format.fprintf ppf "%s" @@ Sexp.to_string @@ sexp_of_t cond

  let to_string cond = Format.asprintf "%a" pp cond

  let pps () cond = to_string cond
end

type z80_cond = Z80_cond.t [@@deriving sexp]



(** ~~~~~~~~~~~~ *)
(** ~~~ Imm8 ~~~ *)
(** ~~~~~~~~~~~~ *)
module Imm8 = struct
  type t = UInt8.t
  (** Make a UInt 8 module that implements sexp *)
  (**
     val t_of_sexp : Sexp.t -> t
     val sexp_of_t : t -> Sexp.t
  *)

  let t_of_sexp sexp = UInt8.of_int (Int.t_of_sexp sexp)
  let sexp_of_t uint = Int.sexp_of_t (UInt8.to_int uint)

  let pp ppf (imm : t) =
    Format.fprintf ppf "0x%02x" @@ UInt8.to_int imm

  let to_string imm =
    Format.asprintf "%a" pp imm

  let pps () imm =
    to_string imm
end

(** A nicer name for Imm8.t. Currently unused. Can't substitute
    below, it breaks compliation (doesn't see imm8_of_sexp. Same for
    reg *)
type imm8 = Imm8.t




(** ~~~~~~~~~~~~ *)
(** ~~~~ Op ~~~~ *)
(** ~~~~~~~~~~~~ *)
module Z80_op = struct
  (** We will have an array of operands and pattern match against it,
      in combination with Insn. *)

  type t =[
    | z80_reg8
    | z80_reg16
    | z80_cond
    | `Imm of word (*TODO imm16 dedicated type to avoid x2 Imm(8)s? *)
  ] [@@deriving sexp_of]

  let pp ppf op =
    match op with
    | `Imm w -> Format.fprintf ppf "%a" Word.pp w
    | x -> Format.fprintf ppf "%a" Sexp.pp (sexp_of_t x)
end

type z80_op = Z80_op.t [@@deriving sexp_of]

module Z80_insn = struct
  type bits = [
    | `BIT (* Tests if the specified bit is set. *)
    | `CPL (* CPL inverts all bits of A.*)
    | `SET (* Sets the specified bit. *)
  ] [@@deriving sexp]

  type mem = [
    (* The LD instruction is used to put the value from one place into
       another place. *)
    | `LD
    (* Does a sort of "LD (DE),(HL)", then decrements DE, HL, and
       BC *)
    | `LDH (* gameboy specific *)
    | `LDHL (* gameboy specific *)
    | `LDD
    (* Repeats the instruction LDD (Does a LD (DE),(HL) and decrements
       each of DE, HL, and BC) until BC=0. Note that if BC=0 before the
       start of the routine, it will try loop around until BC=0 again. *)
    | `LDDR
    (* Performs a "LD (DE),(HL)", then increments DE and HL, and
       decrements BC. *)
    | `LDI
    (* Repeats LDI (LD (DE),(HL), then increments DE, HL, and
       decrements BC) until BC=0. Note that if BC=0 before this
       instruction is called, it will loop around until BC=0 again. *)
    | `LDIR
  ] [@@deriving sexp]

  type branch = [
    | `CALL
    (* Decreases B and jumps to a label if not zero. Note that DJNZ does a
       relative jump, so it can only jump between 128 bytes back/ahead. *)
    | `DJNZ
    (* Absolute jumps to the address. *)
    | `JP
    (* Relative jumps to the address. *)
    | `JR
  ] [@@deriving sexp]

  type general = [
    | `ADC
    | `ADD
    | `AND
    | `CCF (* Invert carry flag *)
    (* CP is a subtraction from A that doesn't update A, only the flags it
       would have set/reset if it really was subtracted. *)
    | `CP
    (* When this instruction is executed, the A register is BCD
       corrected using the contents of the flags. *)
    | `DAA
    | `DEC
    | `DI (* Disable interrupts *)
    | `EI (* Enable interrupts *)
    | `EX (* Exchanges two 16-bit values. *)
    (* EXX exchanges BC, DE, and HL with shadow registers with BC', DE',
       and HL'. *)
    | `EXX
    | `CPD (* Combine 1) CP HL, 2) DEC HL, 3) DEC BC *)
    (* Repeats CPD until either: BC=0 || A=HL*)
    | `CPDR
    (* CPD does these things in this order: 1) CP (HL) 2) INC HL 3) DEC
       BC *)
    | `CPI
    (* Repeats CPD until either: BC=0 || A=HL*)
    | `CPIR
    | `HALT (* Suspends all actions until the next interrupt. *)
    | `IM (* Sets interrupt mode *)
    | `INC
    (* Reads the (C) port and writes the result to (HL), then
       decrements HL and decrements B. *)
    | `IN (* Reads a value from a hardware port *)
    | `IND
    (* Reads the (C) port and writes the result to (HL), then
       increments HL and decrements B. *)
    | `INI
    (* Reads the (C) port and writes the result to (HL). HL and B
       are decremented. Repeats until B = 0. *)
    | `INDR
    (* Reads from the (C) port, then writes to (HL). HL is
       incremented and B is decremented. Repeats until B = 0. *)
    | `INIR
    | `NEG (* Negates the accumulator *)
    | `NOP (* NOP does nothing for 4 clock cycles. *)
    (* Or is an instruction that takes an 8-bit input an compare sit with
       the accumulator. It checks to see if anything is set, and if neither
       are set, it results in a zero. *)
    | `OR
    (* Reads from (HL) and writes to the (C) port. HL and B are
        then decremented. Repeats until B = 0. *)
    | `OTDR
    (* Reads from (HL) and writes to the (C) port. HL is
       incremented and B is decremented. Repeats until B = 0. *)
    | `OTIR
    (* Writes the value of the second operand into the port given by the
       first operand. *)
    | `OUT
    (* Writes the value from (HL) to the (C) port, then decrements
       B and HL. *)
    | `OUTD
    (* Reads from (HL) and writes to the (C) port. HL is then incremented,
       and B is decremented. *)
    | `OUTI
    (* Copies the two bytes from (SP) into the operand, then increases SP
       by 2. *)
    | `POP
    (* Copies the operand into (SP), then increments SP by 2. *)
    | `PUSH
    (* Resets the specified byte to zero. *)
    | `RES
    (* Pops the top of the stack into the program counter. Note that
       RET can be either conditional or unconditional. *)
    | `RET
    (* Returns from an interrupt routine. Note: RETI cannot use return
       conditions. *)
    | `RETI
    (* Returns from the non-maskable interrupt (NMI). Cannot take
       return conditions. *)
    | `RETN
    (* 9-bit rotation to the left. the register's bits are shifted left.
       The carry value is put into 0th bit of the register, and the leaving
       7th bit is put into the carry. *)
    | `RL
    (* Performs an RL A, but is much faster and S, Z, and P/V flags
       are preserved. *)
    | `RLA
    (* 8-bit rotation to the left. The bit leaving on the left is
       copied into the carry, and to bit 0. *)
    | `RLC
    (* Performs RLC A much quicker, and modifies the flags differently. *)
    | `RLCA
    (* Performs a 4-bit leftward rotation of the 12-bit number whose 4
       most signigifcant bits are the 4 least significant bits of A, and its
       8 least significant bits are in (HL). *)
    | `RLD
    (* 9-bit rotation to the right. The carry is copied into bit 7,
       and the bit leaving on the right is copied into the carry. *)
    | `RR
    | `RRA
    | `RRC
    | `RRCA
    | `RRD
    (* The current PC value plus three is pushed onto the stack. The
       MSB is loaded with $00 and the LSB is loaded with imm8. *)
    | `RST
    (* Sum of second operand and carry flag is subtracted from the
       first operand. Results are written into the first operand. *)
    | `SBC
    (* Set carry flag instruction. *)
    | `SCF
    | `SLA
    (* Arithmetic shift right 1 bit, bit 0 goes to carry flag, bit 7
       remains unchanged. *)
    | `SRA
    (* Like SRA, except a 0 is put into bit 7. The bits are all shifted
       right, with bit 0 put into the carry flag. *)
    | `SRL
    (* Apparently SWAP is an added instruction for gameboy color, I
       couldn't find any reference to except
       http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf. Swap low and
       high nibbles *)
    | `SWAP
    (* Sub stands for subtract but only takes one input. It subtracts the
       input from the accumulator and writes back to it. *)
    | `SUB
    (* XOR is an instruction that takes one 8-bit input and compares
       it with the accumulator. XOR is similar to Or, except for one
       thing: only 1 of the 2 test bits can be set or else it will
       result in a zero. The final answer is stored to the
       accumulator. *)
    | `XOR
  ] [@@deriving sexp]

  type t = [
    | general
    | branch
    | bits
    | mem
    | `Undef
  ] [@@deriving sexp]

  (** The Sexp output can be improved later *)
  let pp ppf insn =
    Format.fprintf ppf "%s" @@ Sexp.to_string @@ sexp_of_t insn

  let to_string insn =
    Format.asprintf "%a" pp insn

  let pps () insn =
    to_string insn
end

type z80_insn = Z80_insn.t [@@deriving sexp]
