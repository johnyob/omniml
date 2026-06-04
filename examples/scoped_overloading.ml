(** overloading *)

(* predefine class incr *)
type 'a incr = Incr of 'a ;;
let val_incr = fun x -> Incr x;;
let use_incr = fun x -> match x with (Incr v -> v);;
let incr = fun? x -> use_incr x;;

(* let incr!int x = x + 1 *)
let incr_int = fun x -> x + 1;;
let incr_int' = val_incr incr_int;;
let implicit incr_int';;

(* let incr!bool x = not x *)
let incr_bool = fun x -> if x then false else true;;
let incr_bool' = val_incr incr_bool;;
let implicit incr_bool';;

(* predefine class incr *)
type 'a show = Show of 'a ;;
let val_show = fun x -> Show x;;
let use_show = fun x -> match x with (Show v -> v);;
let show = fun? x -> use_show x;;

(* String module *)
type string;;
external print_int : int -> string;;
external print_bool : bool -> string;;
external concat : string -> string -> string;;

(* let show!int = print_int *)
let show_int = print_int;;
let show_int' = val_show show_int;;
let implicit show_int';;

(* let show!bool = print_bool *)
let show_bool = print_bool;;
let show_bool' = val_show show_bool;;
let implicit show_bool';;

(* let one = show! (incr! 0) *)
let one = show (incr 0);;

(* dynamic overloading *)
(* ambigous:
   
   let show_incr = fun x -> show (incr x);;

*)

let show_incr =
  fun? show incr ->
  let show = use_show show in
  let incr = use_incr incr in
  fun x -> show (incr x);;

(* let two = show_incr 1;; *)
let two = show_incr 1;;

(* tuples *)
(* let show_tuple ?show!a ?show!b x =
      (show_a (fst x)) ^ "," ^ (show_b (snd x)) *)

let show_tuple =
  fun show_a show_b ->
  let show_a = use_show show_a in
  let show_b = use_show show_b in
  fun x ->
    let (fst, snd) = x in concat (show_a fst) (show_b snd);;

let show_tuple = fun? show_a show_b ->
  val_show (show_tuple show_a show_b);;

let implicit show_tuple;;

(* let onetrue = show (1, true) *)
let onetrue = show (1, true);;
