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
      let tree1 = generate_tree (Pre pre_p1) in
      let tree2 = generate_tree (Pre pre_p2) in
      Node (SilentTransition, [tree1; tree2])
  | Parallel (p1, p2) ->
      let tree1 = generate_tree p1 in
      let tree2 = generate_tree p2 in
      Node (SilentTransition, [tree1; tree2])

let execution_tree (process : 'a process) =
  generate_tree process

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
  | Silent -> "Silent"

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
  | (Plus (pre_p1, pre_p2), Plus (pre_q1, pre_q2)) ->
      (bisimulate (snd pre_p1) (snd pre_q1) && bisimulate (snd pre_p2) (snd pre_q2)) ||
      (bisimulate (snd pre_p1) (snd pre_q2) && bisimulate (snd pre_p2) (snd pre_q1))
  | (Parallel (p1, p2), Parallel (q1, q2)) ->
      bisimulate p1 q1 && bisimulate p2 q2
  | _, _ -> false
    

let m1 = Pre (Act "coin", Plus ((Act "tea", Skip), (Act "coffee", Skip)))
let m2 = Plus ((Act "coin", Pre (Act "tea", Skip)), (CoAct "coin", Pre (CoAct "coffee", Skip)))
let m3 = Parallel(Skip,m1)

let _ = 
  let m2m1 = process_simulates m2 m1 in
  let m1m2 = process_simulates m1 m2 in
  Printf.printf "m2m1 = %b\n" m2m1;  (* devrait afficher "false" *)
  Printf.printf "m1m2 = %b\n" m1m2;   (* devrait afficher "true" *)

;;

let test_bisimulation_m1_m2 () = 
  let result = bisimulate m2 m1 in
  if result then
    print_endline "Test de bisimulation réussi : M1 ∼ M2"
  else
    print_endline "Test de bisimulation échoué : M1 et M2 ne sont pas bisimilaires"
;;

test_bisimulation_m1_m2 () ;;
  
let _ = let tree = generate_tree m2 in print_endline (string_of_tree "" tree)
let _ = print_endline "\n";;

let _ = let tree = generate_tree m1 in print_endline (string_of_tree "" tree)
let _ = print_endline "\n";;

let um_parallel = Parallel (Pre (Act "tea", Pre (Act "coin", Skip)), Pre (Act "tea", Pre (Act "coin", Skip)))

let _ = let tree = generate_tree um_parallel in  print_endline (string_of_tree "" tree)
