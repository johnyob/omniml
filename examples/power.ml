external fix : 'a 'b. (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b;;
let power = fix (fun power x n ->
    if n = 0
      then 1
      else x * power x (n - 1))
;;

