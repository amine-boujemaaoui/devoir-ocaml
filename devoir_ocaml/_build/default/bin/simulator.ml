open Action
open Pre_process
open Process

let rec process_simulates p q =
  match (p, q) with
  | (Skip, _) -> true
  | (_, Skip) -> false
  | (Pre (a1, p1), Pre (a2, p2)) -> a1 = a2 && process_simulates p1 p2
  | (Plus (pre_p1, pre_p2), Plus (pre_q1, pre_q2)) ->
    ((process_simulates (snd pre_p1) (snd pre_q1) && process_simulates (snd pre_p2) (snd pre_q2)) ||
    (process_simulates (snd pre_p1) (snd pre_q2) && process_simulates (snd pre_p2) (snd pre_q1)))
  | (Parallel (p1, p2), Parallel (q1, q2)) ->
    (process_simulates p1 q1) && (process_simulates p2 q2)
  | (Pre (a1, p1), Plus (pre_q1, pre_q2)) -> 
    (process_simulates p1 (snd pre_q1) && process_simulates p1 (snd pre_q2))
  | (Plus (pre_p1, pre_p2), Pre (a2, p2)) -> 
    (process_simulates (snd pre_p1) p2 && process_simulates (snd pre_p2) p2)
  | _ -> false

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
