open Core_kernel.Std
open Bap.Std
open Format

open Util.Util_word
open Util.Util_bil

module Z80 = Z80_disasm
module Env = Z80_env

let highlight s =
  let red = "\x1b[41m" in
  let restore = "\x1b[40m" in
  Format.sprintf "%s%s%s" red s restore

let var_of_reg = Env.of_reg
let exp_of_reg = Fn.compose Bil.var var_of_reg

let (!$) = var_of_reg
let (!) = exp_of_reg

(** Whatever the expression is, keep only the lowest bit *)
let cast_lo1 = Bil.(cast LOW 1)
(** Whatever the expression is, keep only the highest bit *)
let cast_hi1 = Bil.(cast HIGH 1)

(** cast an expression to 16 bit width (imm or reg) *)
let to_16 = Bil.(cast UNSIGNED 16)
(** cast an expression to 8 bit width (imm or reg) *)
let to_8 = Bil.(cast UNSIGNED 8)

let lsl1 = Fn.flip (Bil.binop Bil.lshift) (i8 1)

(** Store little endian, 2 bytes *)
let store_to16 ~(dst : exp) ~(src : exp) : stmt =
  let store_to =
    Bil.(store ~mem:(var Env.mem) ~addr:dst src LittleEndian `r16) in
  Bil.(Env.mem := store_to)

(** Load little endian, 2 bytes *)
let load_from16 ~(dst : var) ~(src : exp) : stmt =
  let load_from =
    Bil.(load ~mem:(var Env.mem) ~addr:src LittleEndian `r16) in
  Bil.(dst := load_from)

let store_to ~(dst : exp) ~(src : exp) : stmt =
  let store_to =
    Bil.(store ~mem:(var Env.mem) ~addr:dst src LittleEndian `r8) in
  Bil.(Env.mem := store_to)

let load_from ~(dst : var) ~(src : exp) : stmt =
  let load_from =
    Bil.(load ~mem:(var Env.mem) ~addr:src LittleEndian `r8) in
  Bil.(dst := load_from)

(** fucked up way to cast a 8bit word to 16.. *)
let up16 w =
  Word.to_int w
  |> ok_exn
  |> Word.of_int ~width:16

(** TODO: possibly omit need for +/- by using correct word *)
let calc ~r (w : int) (op : [`ADD | `SUB]) =
  let exp =
    match r, op with
    | #Z80.Reg8.t as r8,`ADD -> Bil.(!r8 + i8 w)
    | #Z80.Reg8.t as r8,`SUB -> Bil.(!r8 - i8 w)
    | #Z80.Reg16.t as r16,`ADD -> Bil.(!r16 + i16 w)
    | #Z80.Reg16.t as r16,`SUB -> Bil.(!r16 - i16 w) in
  Bil.(!$r := exp)

let inc ~r = calc ~r 1 `ADD

let dec ~r = calc ~r 1 `SUB

(** [b] is b0 or b1. [f] is Env.f* *)
let set_flag ~f ~(b : word) = Bil.(f := int b)
let set_1 ~f = set_flag ~f ~b:Word.b1
let set_0 ~f = set_flag ~f ~b:Word.b0

let lift (stmt : Z80.Stmt.t) =
  match stmt with
  | `NOP, _ -> []

  | `XOR, [#Z80.Reg8.t as r] ->
    let exp = Bil.(!r lxor var Env.a) in
    [ Bil.(!$ r := exp)
    ; Bil.(Env.fz := binop EQ !r (i8 0))
    ; set_0 Env.fn
    ; set_0 Env.fh
    ; set_0 Env.fc ]

  | `JP, [`Imm x;`Imm y] ->[Bil.(jmp (int Word.(x@.y)))]

  | `JP, [#Z80.Reg8.t as target] -> [Bil.jmp !target]

  | `JR, [#Z80.Cond.t as c; `Imm x] ->
    let x16 = Bil.((cast SIGNED 16) (int x)) in
    let true_ = [ Bil.(jmp ((var Env.pc) + x16)) ] in
    let false_ = [] in
    begin match c with
      | `FNZ -> [ Bil.(if_ (lnot (var Env.fz)) true_ false_ ) ]
      | `FZ -> [ Bil.(if_ (var Env.fz) true_ false_) ]
      | _ -> [ Bil.special ("Unimplemented" |> highlight) ]
    end

  (* Opposite of the nth bit is written into the Z flag. C is preserved, N is
     reset, H is set, and S and P/V are undefined.*)
  | `JR, [`Imm x;] ->
    let to_s16 = Bil.(cast SIGNED 16) in
    [ Bil.(jmp ((var Env.pc) + to_s16 (int x))) ]

  | `BIT, [`Imm idx; #Z80.Reg8.t as r] ->
    let bit_to_test = Bil.(!r lsr (int idx)) in
    [ Bil.(Env.fh := true_)
    ; Bil.(Env.fn := false_)
    ; Bil.(Env.fz := cast_lo1 @@ lnot bit_to_test) ]

  | `INC, [#Z80.Reg8.t as r] ->
    [ inc ~r
    ; Bil.(Env.fz := !r = (i8 0))
    ; set_0 Env.fn
    ; Bil.(Env.fh := (!r land (i8 0xf)) = (i8 0)) ]

  | `INC, [#Z80.Reg16.t as r] -> [inc ~r]

  | `DEC, [#Z80.Reg8.t as r] ->
    [ dec ~r
    ; Bil.(Env.fz := !r = (i8 0))
    ; set_1 Env.fn
    ; Bil.(Env.fh := (!r land (i8 0xf)) = (i8 0xf)) ]

  | `DEC, [#Z80.Reg16.t as r] -> [dec ~r]

  | `LD, [#Z80.Reg16.t as r; `Imm x; `Imm y] -> [Bil.(!$ r := int Word.(x@.y))]

  | `LD, [#Z80.Reg8.t as r; `Imm addr] -> [Bil.(!$ r := (int addr))]

  | `LD, [#Z80.Reg8.t as r1; #Z80.Reg8.t as r2] -> [Bil.(!$ r1 := !r2)]

  | `LD, [#Z80.Reg8.t as r1; #Z80.Reg16.t as r2] -> [load_from ~dst:!$r1 ~src:!r2]

  (* Generalization to generous? I don't think so: this always means store to
      address of Reg16 *)
  | `LD, [#Z80.Reg16.t as r1; #Z80.Reg8.t as r2 ] -> [store_to ~dst:!r1 ~src:!r2]

  (* LD for special cases where hl is incremented/decremented *)
  | `LD, [#Z80.Reg16.t as r1; `Imm c; #Z80.Reg8.t as r2] ->
    let update =
      match Word.to_int c |> ok_exn with
      | 1 -> inc r1
      | 255 -> dec r1
      | d -> failwith @@ sprintf "Impossible: %d" d in
    [store_to ~dst:!r1 ~src:!r2; update]

  (* 0xFF00 + reg C, reg X. Other (shorter) options: concatenate FF and C, but
      this will make the pattern that I match on less specific, and harder to
      determine where it comes from. *)
  | `LD, [`Imm x; `Imm y; #Z80.Reg8.t as r1; #Z80.Reg8.t as r2] ->
    let w = Word.(y@.x) in
    [store_to ~dst:(Bil.(to_16 !r1 + int w)) ~src:!r2]

  (* (x@.y) <- r. TODO,combine with below with offset 0 *)
  | `LD, [`Imm x; `Imm y; #Z80.Reg8.t as r] ->
    let w = Word.(y@.x) in
    [store_to ~dst:(Bil.int w) ~src:!r]

  (* (x@.y) + z <- r *)
  | `LD, [`Imm x; `Imm y; `Imm z; #Z80.Reg8.t as r] ->
    let w = Word.((y@.x) + up16 z) in (* TODO use cast instead of word conv? *)
    [store_to ~dst:(Bil.int w) ~src:!r]

  (* A = (ff00 + z *)
  | `LD, [#Z80.Reg8.t as r; `Imm x; `Imm y; `Imm z] ->
    let w = Word.((y@.x) + up16 z) in  (* TODO use cast instead of word conv? *)
    [load_from ~dst:!$r ~src:(Bil.int w)]

  (* Thank you http://gameboy.mongenel.com/dmg/opcodes.html *)
  | `CALL, [`Imm x; `Imm y] ->
    [ Bil.(store_to16 ~dst:(var Env.sp) ~src:(var Env.pc))
    ; Bil.(Env.sp := var Env.sp - i16 2)
    (* pc var in Env is not the same as pc var in the interpreter. we do not do
       Env.sp := var Env.sp - int (w16 2) here for that reason. just jump
       directly.*)
    ; Bil.(jmp (int Word.(x@.y))) ]

  | `PUSH,[#Z80.Reg16.t as r] ->
    [ Bil.(store_to16 ~dst:(var Env.sp) ~src:!r)
    ; Bil.(Env.sp := var Env.sp - i16 2) ]

  | `POP, [#Z80.Reg16.t as r] ->
    [ Bil.(Env.sp := var Env.sp + i16 2) (* TODO use inc? *)
    ; Bil.(load_from16 ~dst:!$r ~src:(var Env.sp)) ]

  | `RET, [] ->
    let open Bil in
    let target =
      Bil.load
        ~mem:(var Env.mem)
        ~addr:(var Env.sp)
        LittleEndian `r16
    in
    [ Bil.(Env.sp := var Env.sp + i16 2)
    ; Bil.(jmp target) ]

  (* through carry flag : http://gameboy.mongenel.com/dmg/opcodes.html *)
  | `RL, [#Z80.Reg8.t as r] ->
    let tmp = Var.create "c_flag" bool_t in
    [ Bil.(tmp := var Env.fc)
    ; Bil.(Env.fc := cast_hi1 !r)
    ; Bil.(!$r := (lsl1 !r) lor (to_8 (var tmp)))
    ; Bil.(Env.fz := !r = (i8 0))
    ; set_0 ~f:Env.fn
    ; set_0 ~f:Env.fh ]

  (* Same as `RL `A, but less ticks. Fix later *)
  | `RLA, [#Z80.Reg8.t as r] ->
    let tmp = Var.create "c_flag" bool_t in
    [Bil.(tmp := var Env.fc)
    ; Bil.(Env.fc := cast_hi1 !r)
    ; Bil.(!$r := (lsl1 !r) lor (to_8 (var tmp)))
    ; Bil.(Env.fz := !r = (i8 0))
    ; set_0 ~f:Env.fn
    ; set_0 ~f:Env.fh ]

  | `CP, [`Imm x] -> (* same as SUB but discards result*)
    let tmp = Var.create "tmp" reg8_t in
    [ Bil.(tmp := (var Env.a) - int x)
    ; Bil.(Env.fz := (var tmp) = (i8 0))
    ; set_1 Env.fh
    ; Bil.(
        Env.fh := ((var tmp land (i8 0xf)) > ((var Env.a) land (i8 0xf))))
    ; Bil.(Env.fc := (var tmp) < (i8 0)) ]

  | `CP, [#Z80.Reg16.t as r] ->
    let tmp = Var.create "tmp" reg8_t in
    let load_from =
      Bil.(load ~mem:(var Env.mem) ~addr:!r LittleEndian `r8) in
    [ Bil.(tmp := (var Env.a) - load_from)
    ; Bil.(Env.fz := (var tmp) = (i8 0))
    ; set_1 Env.fh
    ; Bil.(Env.fh :=
             ((var tmp land (i8 0xf)) > ((var Env.a) land (i8 0xf))))
    ; Bil.(Env.fc := (var tmp) < (i8 0)) ]

  (* [r1] is always `A *)
  | `SUB, [#Z80.Reg8.t as r1; #Z80.Reg8.t as r2] ->
    [ Bil.(!$r1 := !r1 - !r2)
    ; Bil.(Env.fz := !r1 = (i8 0))
    ; set_1 Env.fh
    ; Bil.(Env.fh :=
             ((!r1 land (i8 0xf)) > ((var Env.a) land (i8 0xf))))
    ; Bil.(Env.fc := (!r1 < (i8 0))) ]

  (* [r1] is always `A *)
  | `ADD, [#Z80.Reg8.t as r1; #Z80.Reg16.t as r2] ->
    let load_from =
      Bil.(load ~mem:(var Env.mem) ~addr:!r2 LittleEndian `r8) in
    [ Bil.(!$r1 := load_from)
    ; Bil.(Env.fz := !r1 = (i8 0))
    ; set_0 Env.fh
    ; Bil.(Env.fh :=
             ((!r1 land (i8 0xf)) > ((var Env.a) land (i8 0xf))))
    ; Bil.(Env.fc := (!r1 < (i8 0))) ]

  | `Undef, _ -> [ Bil.special ("Undefined" |> highlight) ]

  | _ -> [ Bil.special ("Unimplemented" |> highlight) ]
