external ignore : 'a. 'a -> unit;;

let test = fun (forall id : 'a. 'a -> 'a) -> 
  ignore (id 1);
  ignore (id true);
  ()
;;
