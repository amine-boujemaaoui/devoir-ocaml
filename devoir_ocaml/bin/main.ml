open Action
open Pre_process
open Process
open Transition
open Tree
open Simulator

let () =
  let m1 = Pre (Act "coin", Plus (Act "tea", Skip)) in
  let m2 = Plus (Act "coin", Pre (Act "tea", Skip)) in

  let m2m1 = process_simulates m2 m1 in
  let m1m2 = process_simulates m1 m2 in

  Printf.printf "m2m1: %b\n" m2m1;
  Printf.printf "m1m2: %b\n" m1m2
