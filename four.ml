open Printf

(* My solution *)
let len lst =
  let rec list_length (l: 'a list) (acc: int) =
    match l with
    | [] -> acc
    | _ :: t -> list_length t (acc + 1)
  in list_length lst 0
;;

(* Given solution (tail-recursive [what?]) *)
let length list =
    let rec aux n = function
      | [] -> n
      | _ :: t -> aux (n + 1) t
    in aux 0 list;;

let list_to_string_with_semicolons lst =
  String.concat "; " (List.map string_of_int lst)
;;

let example_list = [1; 2; 3; 4; 5; 6; 7];;

let () =
  print_endline (list_to_string_with_semicolons example_list);
  printf "My count: %d\n" (len example_list);
  printf "Example count: %d\n" (length example_list)
