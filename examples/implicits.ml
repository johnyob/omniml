type string;;

external nil_string : string;;
external cons_string : string -> string -> string;;
external int_show : int -> string;;

external fix : 'a 'b. (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b;;


type 'a list = 
  | Nil
  | Cons of 'a * 'a list
;;

let list_show = fun? showx -> 
  fix (fun list_show xs -> 
    match xs with (
    | Nil -> nil_string
    | Cons (x, xs) -> cons_string (showx x) (list_show xs)))
;;


let implicit int_show;;
let implicit list_show;;

let show = fun? showx -> exists (type 'a) -> fun (x : 'a) -> ((showx : 'a -> string) x);;

let _ = show (Cons (1, Cons (2, Nil)));;
