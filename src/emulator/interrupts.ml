open Core_kernel.Std
open Bap.Std

open Logging

(* TODO remove interp *)
let write_word addr w (ctxt : Z80_interpreter_debugger.context) interp :
Z80_interpreter_debugger.context option =
  let open Option in
  let open Z80_cpu.CPU in
  let store_ addr v = Bil.store ~mem:(Bil.var Z80_cpu.CPU.mem)
      ~addr:(Bil.int addr) (Bil.int v) LittleEndian `r8 in
  let stmt = [Bil.(Z80_cpu.CPU.mem := store_ addr w)] in
  ctxt#lookup (Z80_env.mem) >>= fun result ->
  match Bil.Result.value result with
  | Bil.Mem storage ->
    let start = interp#eval stmt in
    (* TODO may need other interp so things dont get caught later *)
    let ctxt' = Monad.State.exec start ctxt in
    Some ctxt'
  | _ -> None

let to_string = function
  | 0 -> "v-blank"
  | 1 -> "lcd"
  | 2 -> "timer"
  | 3 -> "joypad"
  | _ -> failwith "Unknown interrupt"

let request interp ctxt i =
  let open Option in
  let open Util in
  log_interrupt @@ sprintf "New interrupt request %d:%s" i (to_string i);
  ctxt#mem_at_addr (w16 0xFF0F) >>= fun req ->
  let rq = set_bit req i in
  write_word (w16 0xFF0F) rq ctxt interp
