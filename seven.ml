open Printf;;

type 'a node =
  | One of 'a
  | Many of 'a node list;;

(* Used some aspects of the given solution as reference *)
(*  since this one was a little difficult for me. *)
(*  I use a match instead of "function" here though. *)
(*  I also append in the "One" branch instead of prepend *)
(*  to avoid needing to reverse the list afterward. I think *)
(*  my solution is actually just as good if not better for *)
(*  that reason. *)

let flatten lst =
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | One x :: tl -> aux tl (acc @ [x])
    | Many l :: tl -> aux tl (aux l acc)
  in
  aux lst [];;

let () =
  print_endline (match (flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]) with
  | ["a"; "b"; "c"; "d"; "e"] -> "SUCCESS"
  | _ -> "FAILURE :(")
