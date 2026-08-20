open Core
open Omniml_std

module Test_structure = struct
  type 'node t =
    | Variable
    | Structure of 'node list
  [@@deriving sexp_of]

  let iter t ~f =
    match t with
    | Variable -> ()
    | Structure children -> List.iter children ~f
  ;;

  type 'node ctx = unit

  exception Cannot_merge

  let merge ~ctx:() ~create:_ ~unify ~type1:_ ~type2:_ t1 t2 =
    let unify_list ts1 ts2 =
      match List.iter2 ts1 ts2 ~f:unify with
      | Ok () -> ()
      | Unequal_lengths -> raise Cannot_merge
    in
    match t1, t2 with
    | Variable, t | t, Variable -> t
    | Structure ts1, Structure ts2 ->
      unify_list ts1 ts2;
      t1
  ;;

  module Region_metadata = struct
    type 'node t = string [@@deriving sexp_of]
  end
end

module G = Omniml_collector.Make (Test_structure)

module Test = struct
  type t =
    { state : G.State.t
    ; root : G.Region.t
    }

  let create () =
    let id_source = Identifier.create_source () in
    let state = G.State.create ~id_source ~root:"root" in
    { state; root = G.State.root_region state }
  ;;

  let region t name = G.Region.create ~state:t.state ~parent:t.root name

  let node ?region t structure =
    G.Node.create
      ~state:t.state
      ~curr_region:(Option.value region ~default:t.root)
      structure
  ;;

  let root t node =
    G.Node.Rooting.root ~state:t.state node;
    node
  ;;

  let unroot t node = G.Node.Rooting.unroot ~state:t.state node

  let collect ?(finalize = ignore) t =
    G.collect_all_regions
      ~state:t.state
      ~before_mark:ignore
      ~before_sweep:ignore
      ~promote:ignore
      ~finalize
      ~after_sweep:ignore
      ()
  ;;

  let unify t ?(curr_region = t.root) left right =
    G.Node.unify ~state:t.state ~curr_region ~ctx:() left right
  ;;
end

let ids = Identifier.create_source ()
let target () = Identifier.create ids

let%test_unit "rootings reach children" =
  let t = Test.create () in
  let child = Test.node t Variable in
  let parent = Test.node t (Structure [ child ]) in
  let root = Test.root t parent in
  Test.collect t;
  assert (G.Node.is_live child);
  Test.unroot t root;
  Test.collect t;
  assert (G.Node.is_dead parent);
  assert (G.Node.is_dead child)
;;

let%test_unit "region updates continue through nodes owned by an ancestor" =
  let t = Test.create () in
  let inner = Test.region t "inner" in
  let child = Test.node ~region:inner t Variable in
  let ancestor = Test.node t (Structure [ child ]) in
  let entry = Test.node ~region:inner t (Structure [ ancestor ]) in
  let root = Test.root t (Test.node t Variable) in
  Test.unify t root entry;
  Test.collect t;
  assert (G.Node.is_live child);
  assert (
    G.Region.Level.equal (G.Region.level (G.Node.region child)) (G.Region.level t.root))
;;

let%test_unit "dead nodes cannot unify" =
  let t = Test.create () in
  let dead = Test.node t Variable in
  Test.collect t;
  assert (G.Node.is_dead dead);
  let live = Test.node t Variable in
  assert (Exn.does_raise (fun () -> Test.unify t dead live))
;;

let%test_unit "rooting during finalization keeps a node live" =
  let t = Test.create () in
  let node = Test.node t Variable in
  (* [finalize] will root [node] twice *)
  let root1 = ref None in
  let root2 = ref None in
  let finalize candidate =
    if G.Node.same_class candidate node && Option.is_none !root1 && Option.is_none !root2
    then (
      root1 := Some (Test.root t node);
      root2 := Some (Test.root t node))
  in
  Test.collect ~finalize t;
  assert (G.Node.is_live node);
  Test.unroot t (Option.value_exn ~here:[%here] !root1);
  Test.collect t;
  (* Still live due to [root2] *)
  assert (G.Node.is_live node);
  Test.unroot t (Option.value_exn ~here:[%here] !root2);
  Test.collect ~finalize t;
  (* Dead, no-longer rooted by root1 or root2 *)
  assert (G.Node.is_dead node)
;;

let%test_unit "unification promotes to the nearest common ancestor" =
  let t = Test.create () in
  let inner = Test.region t "inner" in
  (* Create two regions with two nodes:

       Router = [ outer_node ]

            \

              Rinner = [ inner_node ]

     Unifying the two nodes should move the inner node to
     [Router] (aka [t.root]). *)
  let outer_node = Test.node ~region:t.root t Variable in
  let inner_node = Test.node ~region:inner t Variable in
  Test.unify t ~curr_region:inner outer_node inner_node;
  assert (phys_equal (G.Node.region outer_node) t.root)
;;

let%test_unit "cross-region rooting removal wakes the older region" =
  let t = Test.create () in
  let inner = Test.region t "inner" in
  let older = Test.node t Variable in
  let younger = Test.node ~region:inner t (Structure [ older ]) in
  let root = Test.root t younger in
  Test.collect t;
  assert (G.Node.is_live older);
  assert (G.Node.is_live younger);
  Test.unroot t root;
  Test.collect t;
  assert (G.Node.is_dead older);
  assert (G.Node.is_dead younger)
;;

let%test_unit "cross-region derived roots keep older descendants live" =
  let t = Test.create () in
  let inner = Test.region t "inner" in
  let child = Test.node t Variable in
  let older = Test.node t (Structure [ child ]) in
  let younger = Test.node ~region:inner t (Structure [ older ]) in
  let root = Test.root t younger in
  Test.collect t;
  assert (G.Node.is_live younger);
  assert (G.Node.is_live older);
  assert (G.Node.is_live child);
  Test.unroot t root;
  Test.collect t;
  assert (G.Node.is_dead younger);
  assert (G.Node.is_dead older);
  assert (G.Node.is_dead child)
;;
