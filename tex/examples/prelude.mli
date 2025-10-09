type point =
  { x : int
  ; y : int
  }

type gray_point =
  { x : int
  ; y : int
  ; color : int
  }

val one : point
val app : ('a -> 'b) -> 'a -> 'b
val rev_app : 'a -> ('a -> 'b) -> 'b
val pid : < f : 'a. 'a -> 'a >
