open Printf

let my_list = [ 4; 5; 6 ];;

(* Bad (Actually invalid) solution *)
let rec last l = 
  match List.length l with
  | 1 -> List.nth l 0 
  | _ -> match l with
    | [] -> -99999999
    | x :: xs -> last xs
;;

(* Given solution *)
let rec given_last = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> given_last t
;;

(* My preferred solution *)
let rec good_last l =
  match l with
    | [] -> None
    | [x] -> Some x
    | _ :: t -> good_last t
;;

(*
I appreciate the above solution over the given solution because it explicitly states the arguments
The "function" keyword works without explicitly stating that it's taking in one argument and that's 
weird to me.
*)

let () =
  printf "%d\n" (last my_list);
  (match given_last my_list with 
    | Some x -> printf "Given: %d\n" x
    | None -> print_endline "None :("
  );
  (match good_last my_list with 
    | Some x -> printf "Good: %d\n" x
    | None -> print_endline "None :("
  )
;;
