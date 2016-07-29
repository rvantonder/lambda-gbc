#use "topfind";;
#require "lwt.unix";;
#require "lwt";;
#require "lwt.react";;

open Lwt
open Lwt_react
open Format

let go () =
  (* Set the timer *)
  ignore (Lwt_engine.on_timer 0.016666666 true (fun _ ->
    printf "x\n%!"));
  (* The event loop. Make sure it 'does work', like Lwt_io.printl "x" or reading
   * input *)
  let rec loop () = 
    Lwt_io.read_line Lwt_io.stdin >>= fun str ->
    loop ()
  in
  loop ()

let () =
  Lwt_main.run (go ())
