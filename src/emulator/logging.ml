let make = Lwt_log.Section.make

let log section s = Lwt_log.ign_debug_f ~section "%s" s

let gpu_section = make "gpu"
let log_gpu = log gpu_section

let render_section = make "render"
let log_render = log render_section

let interrupt_section = make "interrupt"
let log_interrupt = log interrupt_section

let cycles_section = make "cycles"
let log_cycles = log cycles_section

let clock_section = make "clock"
let log_clock = log clock_section

let ev_cpu_add_bp_section = make "ev_cpu_add_bp"
let log_ev_cpu_add_bp = log ev_cpu_add_bp_section

let ev_cpu_rq_snd_section = make "ev_cpu_rq_snd"
let log_ev_cpu_rq_snd = log ev_cpu_rq_snd_section

let ev_cpu_bp_trigger_section = make "ev_cpu_bp_trigger"
let log_ev_cpu_bp_trigger = log ev_cpu_bp_trigger_section

let ev_cpu_dbg_eval_section = make "ev_cpu_dbg_eval"
let log_ev_cpu_dbg_eval = log ev_cpu_dbg_eval_section

let ev_cpu_dbg_pc_undef_section = make "ev_cpu_dbg_pc_undef"
let log_ev_cpu_dbg_pc_undef = log ev_cpu_dbg_pc_undef_section

let ev_dbg_rq_rcv_section = make "ev_dbg_rq_rcv"
let log_ev_dbg_rq_rcv = log ev_dbg_rq_rcv_section

let ev_dbg_rq_rcv_in_blking_section = make "ev_dbg_rq_rcv_in_blking"
let log_ev_dbg_rq_rcv_in_blking = log ev_dbg_rq_rcv_in_blking_section

let ev_dbg_rq_rcv_in_non_blking_section =
  make "ev_dbg_rq_rcv_in_non_blking"

let log_ev_dbg_rq_rcv_in_non_blking =
  log ev_dbg_rq_rcv_in_non_blking_section
