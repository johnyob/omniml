type point =
  { x : int
  ; y : int
  }

type gray_point =
  { x : int
  ; y : int
  ; color : int
  }

let one = { x = 42; y = 1337 }
let app f x = f x
let rev_app x f = f x

let pid =
  object
    method f : 'a. 'a -> 'a = fun x -> x
  end
;;
