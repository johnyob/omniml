type 'a list = 
  | Nil
  | Cons of 'a * 'a list
;;

let bad = 
  fun? self -> exists (type 'a) -> 
  let _ = (self : 'a list list) in (Nil : 'a list)
;;

let implicit bad;;

let summon = fun? x -> x;;
let _ = (summon : int list);;
