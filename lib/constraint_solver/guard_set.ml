open! Import
module Transitive_guard = Identifier

type t =
  { direct_guards : int
  ; transitive_guards : int Transitive_guard.Map.t
  }
[@@deriving sexp_of]

let empty = { direct_guards = 0; transitive_guards = Transitive_guard.Map.empty }
let is_empty t = t.direct_guards = 0 && Map.is_empty t.transitive_guards

let add_transitive_guard t tg =
  { t with
    transitive_guards =
      Map.update
        t.transitive_guards
        tg
        ~f:(Option.value_map ~default:1 ~f:(fun x -> x + 1))
  }
;;

let remove_transitive_guard t tg =
  { t with
    transitive_guards =
      Map.change t.transitive_guards tg ~f:(function
        | None ->
          invalid_argf
            "Guard_set.remove_transitive_guard: transitive guard %d not in set"
            (tg :> int)
            ()
        | Some n ->
          if n <= 0 then assert false;
          if n = 0 then None else Some (n - 1))
  }
;;

let clear_transitive_guard t tg =
  { t with transitive_guards = Map.remove t.transitive_guards tg }
;;

let is_transitively_guarded t ~by =
  try Map.find_exn t.transitive_guards by > 0 with
  | _ -> false
;;

let add_guard t = { t with direct_guards = t.direct_guards + 1 }

let remove_guard t =
  if t.direct_guards > 0 then { t with direct_guards = t.direct_guards - 1 } else t
;;

let union t1 t2 =
  { direct_guards = t1.direct_guards + t2.direct_guards
  ; transitive_guards =
      Map.merge_skewed
        t1.transitive_guards
        t2.transitive_guards
        ~combine:(fun ~key:_ n1 n2 -> n1 + n2)
  }
;;
