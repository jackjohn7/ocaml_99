open Printf

let list_to_string_with_semicolons lst =
  String.concat "; " lst
;;

(* My first solution *)
(*  The most basic inefficiency here is that it tracks *)
(*   the previous value. It was just annoying to get it *)
(*   working without it. It also only works with strings :( *)
let compress l =
  let rec aux lst acc prev =
    match lst with
    | [] -> acc
    | v :: tl -> if v = prev then aux tl acc v else aux tl (acc @ [v]) v
  in
  aux l [] ""
;;

(* Solution given by ocaml.org *)
(*  This solution is so nice omg. It doesn't require *)
(*   any accumulator or tracking of previous value. *)
let rec given_compress = function
    | a :: (b :: _ as t) -> if a = b then given_compress t else a :: given_compress t
    | smaller -> smaller;;

let () =
  print_endline (match compress ["a"; "a"; "b"; "b"; "b"; "c"; "d"; "d"] with
  | ["a"; "b"; "c"; "d"] as res -> String.concat ": " ["SUCCESS"; list_to_string_with_semicolons res]
  | res -> String.concat ": " ["FAILURE"; list_to_string_with_semicolons res])
;;
