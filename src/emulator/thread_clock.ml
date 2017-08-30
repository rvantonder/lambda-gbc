open Lwt
open Format

open Logging

let last_time = ref 0.0

let update_time () =
  log_clock @@ sprintf "Last: %f seconds\n" (Unix.gettimeofday () -. !last_time);
  last_time := Unix.gettimeofday ()

let float_of_now () = Unix.gettimeofday ()

let rec sync freq (t, was) =
  Lwt_unix.yield () >>= fun _ ->
  let now = float_of_now () in
  (* unix timestamp. f: fractional part of second. s: integer part. *)
  let f, s  = modf was in
  let f', s' = modf now in
  let (!) = int_of_float in
  if !(f'*.freq) != !(f*.freq) then return (((s' -. s) +. t), now)
  else sync freq (t, now)

let rec loop ticker notify freq =
  sync freq ticker >>= fun ticker ->
  update_time ();
  Lwt_mvar.put notify () >>= fun _ ->
  loop ticker notify freq

let start notify =
  let freq = 60. in
  loop (float_of_now (), float_of_now ()) notify freq
