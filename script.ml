type 'a action =
  | Act of 'a
  | CoAct of 'a
  | Silent

type 'a process =
  | Skip
  | Action of 'a action
  | Plus of 'a process * 'a process
  | Parallel of 'a process * 'a process

let rec process_simulates pre_p p =
  match (pre_p, p) with
  | (Skip, _) -> true  (* Any process can simulate Skip *)
  | (Action pre_action, Action p_action) -> pre_action = p_action  (* Action in pre-process should match Action in process *)
  | (Plus (pre_p1, pre_p2), Plus (p1, p2)) ->
    process_simulates pre_p1 p1 && process_simulates pre_p2 p2  (* Both components should simulate their counterparts *)
  | (Parallel (pre_p1, pre_p2), Parallel (p1, p2)) ->
    process_simulates pre_p1 p1 && process_simulates pre_p2 p2  (* Both components should simulate their counterparts *)
  | _ -> false  (* In all other cases, pre_p does not simulate p *)

let simulates p q =
  process_simulates q p

let coin = Action (Act "coin")
let tea = Action (Act "tea")
let coffee = Action (Act "coffee")

let m1 = Plus (Parallel (coin, Skip), Parallel (tea, Skip))
let m2 = Plus (Parallel (Parallel (coin, tea), Skip), Parallel (Parallel (coin, coffee), Skip))

let m1m2 = simulates m1 m2
let m2m1 = simulates m2 m1

(* Output the result *)
let _ =  Printf.printf "M1 ⪯ M2: %b\n" m1m2
let _ =  Printf.printf "M2 ⪯ M1: %b\n" m2m1
