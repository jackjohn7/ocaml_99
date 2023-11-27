open Printf

(* Solutions are identical *)
let is_palindrome list = list = List.rev list;;

(* I find it super interesting that comparing lists compares values *)
(*  rather than reference. I wonder if that says something about *)
(*  the nature of functional programming as opposed to imperitive *)
(*  programming where mutable data and object referencing is the norm. *)

let () =
  printf "racecar: %b\n" (is_palindrome ["r"; "a"; "c"; "e"; "c"; "a"; "r"]);
  printf "apple:   %b\n" (is_palindrome ["a"; "p"; "p"; "l"; "e"]);;
