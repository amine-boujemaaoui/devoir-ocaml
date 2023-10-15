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

let rec generate_tree process action =
  match process with
  | Skip -> Leaf Skip
  | Action a -> Leaf (Action a)
  | Pre (a, pre_p) ->
    let tree = generate_tree_pre pre_p action in
    Node (ActionTransition (a), [tree])
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
  | (Act a, rest_pre) ->
    let tree = generate_tree_pre rest_pre action in
    Node (ActionTransition (Act a), [tree])
  | _ -> Leaf Skip

let execution_tree (process : string process) =
  generate_tree process Silent

let rec process_simulates p q =
  match (p, q) with
  | (Skip, _) -> true
  | (_, Skip) -> false
  | (Action a1, Action a2) -> a1 = a2
  | (Plus (pre_p1, pre_p2), Plus (pre_q1, pre_q2)) ->
    ((process_simulates_pre (fst pre_p1) (fst pre_q1) && process_simulates_pre (fst pre_p2) (fst pre_q2)) ||
    (process_simulates_pre (fst pre_p1) (fst pre_q2) && process_simulates_pre (fst pre_p2) (fst pre_q1)))
  | (Parallel (p1, p2), Parallel (q1, q2)) ->
    (process_simulates p1 q1) && (process_simulates p2 q2)
  | _ -> false

and process_simulates_pre pre_p pre_q =
  match (pre_p, pre_q) with
  | ((Act a1, rest_p), (Act a2, rest_q)) ->
    (a1 = a2) && (process_simulates_pre rest_p rest_q)
  | _ -> true

let rec string_of_tree = function
  | Leaf process -> string_of_process process
  | Node (transition, trees) -> 
    "(" ^ (string_of_transition transition) ^ " . [" ^ (String.concat " + " (List.map string_of_tree trees)) ^ "])"

and string_of_process = function
  | Skip -> "Skip"
  | Action action -> string_of_action action
  | Plus (pre_p1, pre_p2) -> "(" ^ (string_of_pre_process pre_p1) ^ " + " ^ (string_of_pre_process pre_p2) ^ ")"
  | Parallel (p1, p2) -> "(" ^ (string_of_process p1) ^ " || " ^ (string_of_process p2) ^ ")"

and string_of_action = function
  | Act a -> a
  | CoAct a -> "not " ^ a
  | Silent -> "Silent"

and string_of_pre_process pre_process =
  let (a, p) = pre_process in
  string_of_action a ^ "." ^ string_of_process p

and string_of_transition = function
  | ActionTransition action -> string_of_action action
  | SilentTransition -> "SilentTransition"

let coin = "coin"
let tea = "tea"
let coffee = "coffee"

let m1 = Plus (Pre (coin, Plus (Pre (tea, Skip), Plus (Pre (coin, Plus (Pre (coffee, Skip), Skip)), Skip))))
let m2 = Plus (Pre (coin, Plus (Pre (tea, Skip), Plus (Pre (coin, Plus (Pre (coffee, Skip), Skip)), Skip))))

let _ = print_endline (string_of_bool (process_simulates m2 m1))  (* devrait afficher "true" *)
let _ = print_endline (string_of_bool (process_simulates m1 m2))  (* devrait afficher "true" *)

let _ = 
  let tree = execution_tree m1 in
  print_endline (string_of_tree tree)

let _ = 
  let tree = execution_tree m2 in
  print_endline (string_of_tree tree)
