type t = 
  | A
;; 

type u = 
  | A
;; 

let x = (A : t) ;;
let y = (A : u) ;;

(* The below causes a disambiguation error *)
(* let z = A ;; *)

(* An example of partial generalization in action *)

type r = 
  | K of int 
;; 

type s = 
  | K of int 
;; 

let a = 
  fun old ->
    let g = fun x -> 1 + old (K x) in
    (g 0, (old : r -> int))
;;

(* An example of arbitrary direction propagation *)

type m = 
  | L 
;; 

type n = 
  | L 
;; 

let x1 = (fun (z : m) -> 1) L ;; 

let y1 = fun z -> (z : m -> int) L ;;

let z1 = (fun z -> match z with (L -> 1)) (L : m) ;;


