open Printf

let list_to_string_with_semicolons lst =
  String.concat "; " (List.map string_of_int lst)
;;

(*
  My solution was nearly identical to given one
  With the only difference being the addition of 
  "| [_]" from the given solution that I hadn't 
  accounted for. That and I used a match statement
  instead of "function"
*)
let rec last_two lst =
  match lst with
  | [] | [_] -> None
    | [x; y] -> Some lst
    | _ :: tl -> last_two tl
;;

let () =
  print_endline (match last_two [1; 2; 3; 4; 5; 6] with
    | None -> "L"
    | Some x -> list_to_string_with_semicolons x
  );;

