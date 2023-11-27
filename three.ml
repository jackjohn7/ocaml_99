open Printf

(* Using natural numbers (idx from 1) *)
let rec nth lst idx =
  match idx with
    | 1 -> (match lst with
      | [] -> None
      | first :: _ -> Some first)
    | _ -> (match lst with
      | [] -> None
      | _ :: tl -> nth tl (idx - 1))

(* My second attempt *)
let rec nth2 lst idx =
  match lst with
    | [] -> None
    | first :: tail ->
      if idx = 1 then Some first else nth2 tail (idx - 1)
;;

(* Given solution *)
let rec at k = function
    | [] -> None
    | h :: t -> if k = 1 then Some h else at (k - 1) t;;

let () =
  print_endline (match nth [1; 2; 3; 4; 5; 6] 3 with
    | None -> "None :("
    | Some x -> sprintf "%d" x);
  print_endline (match nth [1; 2; 3; 4; 5; 6] 5 with
    | None -> "None :("
    | Some x -> sprintf "%d" x);
  print_endline (match nth2 [1; 2; 3; 4; 5; 6] 3 with
    | None -> "None :("
    | Some x -> sprintf "%d" x);
  print_endline (match nth2 [1; 2; 3; 4; 5; 6] 5 with
    | None -> "None :("
    | Some x -> sprintf "%d" x)
;;
