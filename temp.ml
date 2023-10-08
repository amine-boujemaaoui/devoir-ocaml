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
