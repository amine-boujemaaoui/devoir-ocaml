type 'a action =
  | Act of 'a
  | CoAct of 'a
  | Silent

type 'a process =
  | Skip
  | Action of 'a action
  | Plus of 'a pre_process * 'a pre_process
  | Parallel of 'a process * 'a process

and 'a pre_process =
  | Pre of 'a * 'a pre_process
  | Or of 'a * 'a process
  | Plus of 'a process * 'a process
  | Empty

type 'a transition =
  | ActionTransition of 'a action
  | SilentTransition

type 'a tree =
  | Leaf of 'a process
  | Node of 'a transition * 'a tree list

let rec generate_tree process action =
  match process with
  | Skip -> Leaf Skip
  | Action a -> Leaf (Action a)
  | Plus (pre_p1, pre_p2) ->
    let tree1 = generate_tree_pre pre_p1 action in
    let tree2 = generate_tree_pre pre_p2 action in
    Node (ActionTransition action, [Node (ActionTransition action, [tree1]); Node (ActionTransition action, [tree2])])
  | Parallel (p1, p2) ->
    let tree1 = generate_tree p1 action in
    let tree2 = generate_tree p2 action in
    Node (SilentTransition, [tree1; tree2])

and generate_tree_pre pre_process action =
  match pre_process with
  | Pre (a, rest_pre) ->
    let tree = generate_tree_pre rest_pre action in
    Node (ActionTransition (Act a), [tree])
  | Empty -> Leaf Skip

let execution_tree (process : string process) =
  generate_tree process Silent

let rec process_simulates p q =
  match (p, q) with
  | (Skip, _) -> true
  | (_, Skip) -> false
  | (Action a1, Action a2) -> a1 = a2
  | (Plus (pre_p1, pre_p2), Plus (pre_q1, pre_q2)) ->
    ((process_simulates_pre pre_p1 pre_q1 && process_simulates_pre pre_p2 pre_q2) ||
    (process_simulates_pre pre_p1 pre_q2 && process_simulates_pre pre_p2 pre_q1))
  | (Parallel (p1, p2), Parallel (q1, q2)) ->
    (process_simulates p1 q1) && (process_simulates p2 q2)
  | _ -> false

and process_simulates_pre pre_p pre_q =
  match (pre_p, pre_q) with
  | (Empty, _) -> true
  | (_, Empty) -> false
  | (Pre (a1, rest_p), Pre (a2, rest_q)) ->
    (a1 = a2) && (process_simulates_pre rest_p rest_q)
  | (Or (a, p), Or (b, q)) -> 
    (a = b) && (process_simulates p q)
  | (Plus (p1, p2), Plus (q1, q2)) ->
    ((process_simulates p1 q1 && process_simulates p2 q2) ||
    (process_simulates p1 q2 && process_simulates p2 q1))
  | _ -> false

let process_simulates_pre_process pre_p pre_q =
  match (pre_p, pre_q) with
  | (Pre (a1, rest_p), Pre (a2, rest_q)) ->
    (a1 = a2) && (process_simulates_pre rest_p rest_q)
  | _ -> false

let rec string_of_tree = function
  | Leaf process -> string_of_process process
  | Node (transition, trees) -> 
    (string_of_transition transition) ^ "." ^ (String.concat " + " (List.map string_of_tree trees))

and string_of_process = function
  | Skip -> "Skip"
  | Action action -> string_of_action action
  | Plus (pre_p1, pre_p2) -> "(" ^ (string_of_pre_process pre_p1) ^ " + " ^ (string_of_pre_process pre_p2) ^ ")"
  | Parallel (p1, p2) -> "(" ^ (string_of_process p1) ^ " || " ^ (string_of_process p2) ^ ")"

and string_of_action = function
  | Act a -> a
  | CoAct a -> "not " ^ a
  | Silent -> "Silent"

and string_of_pre_process = function
  | Pre (a, pre_process) -> a ^ " . " ^ (string_of_pre_process pre_process)
  | Empty -> "Empty"

and string_of_transition = function
  | ActionTransition action -> string_of_action action
  | SilentTransition -> ""

let coin = "coin"
let tea = "tea"
let coffee = "coffee"
(*
let u = Action (Act tea), Action (Act coin), Skip
let m = Action (CoAct tea), Action (CoAct coin), Skip

let um_parallel = Parallel (u, m)

let _ = let tree = execution_tree um_parallel in print_endline (string_of_tree tree)
*)
let m1_pre_process = Or (coin, Or (tea, coffee))
let m2_pre_process = Pre (coin, Plus (Pre (tea, Empty), Pre (coffee, Empty)))

let _ = print_endline (string_of_bool (process_simulates_pre m2_pre_process m1_pre_process))  (* devrait afficher "true" *)
let _ = print_endline (string_of_bool (process_simulates_pre m1_pre_process m2_pre_process))  (* devrait afficher "false" *)

let _ = let tree = execution_tree m1_pre_process in print_endline (string_of_tree tree)
let _ = let tree = execution_tree m2_pre_process in print_endline (string_of_tree tree)