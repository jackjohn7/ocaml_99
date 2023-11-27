open Printf

(* My solution *)
let rev lst =
  let rec rev_with_idx l acc =
    match l with
    | [] -> acc
    | h :: t -> rev_with_idx (t) (h :: acc)
  in
  rev_with_idx lst []
;;

(* Given solution *)
let given_rev list =
    let rec aux acc = function
      | [] -> acc
      | h :: t -> aux (h :: acc) t in
    aux [] list;;

let list_to_string_with_semicolons lst =
  String.concat "; " (List.map string_of_int lst)
;;

let example_list = [1; 2; 3; 4; 5; 6]

let () =
  print_endline (list_to_string_with_semicolons example_list);
  printf "My rev: [%s]\n" (list_to_string_with_semicolons (rev example_list));
  printf "Given:  [%s]\n" (list_to_string_with_semicolons (given_rev example_list))
;;
