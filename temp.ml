type 'a action =
  | Act of 'a
  | CoAct of 'a
  | Silent

type 'a pre_process =
  | Pre of 'a * 'a pre_process
  | Empty

type 'a process =
  | Skip
  | Action of 'a action
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
  | Plus (pre_p1, pre_p2) ->
    let tree1 = generate_tree_pre pre_p1 action in
    let tree2 = generate_tree_pre pre_p2 action in
    Node (ActionTransition Silent, [Node (ActionTransition (Act action), [tree1]); Node (ActionTransition (Act action), [tree2])])
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

let execution_tree process =
  generate_tree process Silent

(* Define a function to extract the action from a pre-process *)
let rec get_action_from_pre pre_p =
  match pre_p with
  | Pre (action, _) -> action
  | Empty -> Silent  (* Return Silent for an empty pre-process *)

(* Define a function to check if a process simulates a pre-process *)
let rec process_simulates pre_p p =
  match (pre_p, p) with
  | (Empty, _) -> true  (* Any process can simulate an empty pre-process *)
  | (Pre (pre_action, _), Action p_action) -> pre_action = p_action  (* Action in pre-process should match Action in process *)
  | (Pre (pre_action, _), Plus (pre_p1, pre_p2)) ->
    process_simulates (Pre (pre_action, Empty)) pre_p1 || process_simulates (Pre (pre_action, Empty)) pre_p2  (* Either pre-process should simulate the action *)
  | (Pre (pre_action, _), Parallel (p1, p2)) ->
    process_simulates (Pre (pre_action, Empty)) p1 && process_simulates (Pre (pre_action, Empty)) p2  (* Both parallel components should simulate the action *)
  | _ -> false  (* In all other cases, P does not simulate Q *)

let simulates p q =
  process_simulates q p

let coin = Action (Act "coin")
let tea = Action (Act "tea")
let coffee = Action (Act "coffee")

let m1 = Plus (Parallel (coin, Skip), Parallel (tea, Skip))
let m2 = Plus (Parallel (Parallel (coin, tea), Skip), Parallel (Parallel (coin, coffee), Skip))

let result = simulates m1 m2

(* Output the result *)
let () =
  Printf.printf "M1 ⪯ M2: %b\n" result
