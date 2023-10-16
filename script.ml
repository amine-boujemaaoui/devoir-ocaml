

type 'a action =
  | Act of 'a
  | CoAct of 'a
  | Silent

type 'a pre_process = 
  'a action * 'a process

and 'a process =
  | Skip
  | Pre of 'a pre_process
  | Plus of 'a pre_process * 'a pre_process
  | Parallel of 'a process * 'a process

type 'a transition =
  | ActionTransition of 'a action
  | SilentTransition

type 'a tree =
  | Leaf of 'a process
  | Node of 'a transition * 'a tree list

let rec generate_tree process =
  match process with
  | Skip -> Leaf Skip
  | Pre (a, p) -> 
      let tree = generate_tree p in
      Node (ActionTransition a, [tree])
  | Plus (pre_p1, pre_p2) ->
      let tree1 = generate_tree (snd pre_p1) in
      let tree2 = generate_tree (snd pre_p2) in
      Node (SilentTransition, [tree1; tree2])
  | Parallel (p1, p2) ->
      let tree1 = generate_tree p1 in
      let tree2 = generate_tree p2 in
      Node (SilentTransition, [tree1; tree2])

let execution_tree (process : 'a process) =
  generate_tree process

let rec string_of_tree = function
  | Leaf process -> string_of_process process
  | Node (transition, trees) -> 
      "(" ^ (string_of_transition transition) ^ " . [" ^ (String.concat " + " (List.map string_of_tree trees)) ^ "])"

and string_of_process = function
  | Skip -> "Skip"
  | Pre (action, process) -> string_of_action action ^ "." ^ string_of_process process
  | Plus (pre_p1, pre_p2) -> "(" ^ (string_of_pre_process pre_p1) ^ " + " ^ (string_of_pre_process pre_p2) ^ ")"
  | Parallel (p1, p2) -> "(" ^ (string_of_process p1) ^ " || " ^ (string_of_process p2) ^ ")"

and string_of_action = function
  | Act a -> a
  | CoAct a -> "not " ^ a
  | Silent -> "Silent"

and string_of_pre_process (action, process) =
  string_of_action action ^ "." ^ string_of_process process

and string_of_transition = function
  | ActionTransition action -> string_of_action action
  | SilentTransition -> "SilentTransition"

let rec process_simulates p q =
  match (p, q) with
  | (Skip, _) -> true
  | (_, Skip) -> false
  | (Pre (a1, pre_p), Pre (a2, pre_q)) when a1 = a2 -> 
      process_simulates pre_p pre_q
  | (Pre (a, pre_p), Plus (pre_q1, pre_q2)) ->
      (a = fst pre_q1 && process_simulates pre_p (snd pre_q1)) ||
      (a = fst pre_q2 && process_simulates pre_p (snd pre_q2))
  | (Plus (pre_p1, pre_p2), _) ->
      process_simulates (snd pre_p1) q && process_simulates (snd pre_p2) q
  | (Parallel (p1, p2), Parallel (q1, q2)) ->
      (process_simulates p1 q1 && process_simulates p2 q2) ||
      (process_simulates p1 q2 && process_simulates p2 q1)
  | _, _ -> false

let m1 = Pre (Act "coin", Plus ((Act "tea", Skip), (Act "coffee", Skip)))
let m2 = Plus ((Act "coin", Pre (Act "tea", Skip)), (Act "coin", Pre (Act "coffee", Skip)))




let _ = 
let m2m1 = process_simulates m2 m1 in
let m1m2 = process_simulates m1 m2 in
Printf.printf "m2m1 = %b\n" m2m1;  (* devrait afficher "false" *)
Printf.printf "m1m2 = %b\n" m1m2;   (* devrait afficher "true" *)
Printf.printf "Arbre m1:\n%s\n" (string_of_tree (execution_tree m1));
Printf.printf "Arbre m2:\n%s\n" (string_of_tree (execution_tree m2));