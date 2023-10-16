open Types

let rec generate_tree process =
  match process with
  | Skip -> Leaf Skip
  | Pre (a, p) -> 
      let tree = generate_tree p in
      Node (ActionTransition a, [tree])
  | Plus (pre_p1, pre_p2) ->
      let tree1 = generate_tree (Pre pre_p1) in
      let tree2 = generate_tree (Pre pre_p2) in
      Node (SilentTransition, [tree1; tree2])
  | Parallel (p1, p2) ->
      let tree1 = generate_tree p1 in
      let tree2 = generate_tree p2 in
      Node (SilentTransition, [tree1; tree2])

let rec string_of_tree indent = function
  | Leaf process -> indent ^ string_of_process process
  | Node (transition, trees) -> 
      match transition with
      | SilentTransition -> 
          String.concat "\n" (List.map (string_of_tree (indent ^ "| ")) trees)
      | ActionTransition action -> 
          indent ^ "+- " ^ (string_of_action action) ^ "\n" ^
          (String.concat "\n" (List.map (string_of_tree (indent ^ "| ")) trees))

and string_of_process = function
  | Skip -> "Skip"
  | Pre (action, process) -> string_of_action action ^ " -> " ^ string_of_process process
  | Plus (pre_p1, pre_p2) -> "(" ^ (string_of_pre_process pre_p1) ^ " + " ^ (string_of_pre_process pre_p2) ^ ")"
  | Parallel (p1, p2) -> "(" ^ (string_of_process p1) ^ " || " ^ (string_of_process p2) ^ ")"

and string_of_action = function
  | Act a -> a
  | CoAct a -> "not(" ^ a ^ ")"

and string_of_pre_process (action, process) =
  string_of_action action ^ " -> " ^ string_of_process process
    
let rec bisimulate p q =
  match (p, q) with
  | (Skip, Skip) -> true
  | (Pre (a1, pre_p), Pre (a2, pre_q)) when a1 = a2 -> bisimulate pre_p pre_q
  | (Plus (pre_p1, pre_p2), Plus (pre_q1, pre_q2)) ->
      (bisimulate (snd pre_p1) (snd pre_q1) && bisimulate (snd pre_p2) (snd pre_q2)) ||
      (bisimulate (snd pre_p1) (snd pre_q2) && bisimulate (snd pre_p2) (snd pre_q1))
  | (Parallel (p1, p2), Parallel (q1, q2)) ->
      bisimulate p1 q1 && bisimulate p2 q2
  | (_, _) -> false

    

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