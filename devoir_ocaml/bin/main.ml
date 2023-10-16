open Types
open Functions

(* declaration des processus *)
let m1 = Pre (Act "coin", Plus ((Act "tea", Skip), (Act "coffee", Skip)))
let m2 = Plus ((Act "coin", Pre (Act "tea", Skip)), (CoAct "coin", Pre (CoAct "coffee", Skip)))
let um_parallel = Parallel (Pre (Act "tea", Pre (Act "coin", Skip)), Pre (Act "tea", Pre (Act "coin", Skip)));;

(* test de la fonction process_simulates *)
let _ = 
  let m1m2 = process_simulates m1 m2 in
  let m2m1 = process_simulates m2 m1 in
  Printf.printf "M2 ⪯ M1 = %b\n" m1m2;  (* devrait afficher "true" *)
  Printf.printf "M1 ⪯ M2 = %b\n" m2m1;  (* devrait afficher "false" *)
;;
print_endline "\n"

(* test de la fonction bisimulate *)
let test_bisimulation_m1_m2 () = 
  let result = bisimulate m2 m1 in
  if result then
    print_endline "Test de bisimulation réussi : M1 ∼ M2\n"
  else
    print_endline "Test de bisimulation échoué : M1 et M2 ne sont pas bisimilaires\n"
;;
test_bisimulation_m1_m2 () ;;


(* test de la fonction generate_tree *)
print_endline "Arbre d'execution de m1"
let _ = let tree = generate_tree m1 in print_endline (string_of_tree "" tree)
let _ = print_endline "\n";;

print_endline "Arbre d'execution de m2"
let _ = let tree = generate_tree m2 in print_endline (string_of_tree "" tree)
let _ = print_endline "\n";;

print_endline "Arbre d'execution de U || M"
let _ = let tree = generate_tree um_parallel in  print_endline (string_of_tree "" tree)
let _ = print_endline "\n";;
