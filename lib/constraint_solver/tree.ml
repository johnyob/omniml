open! Import

module Level : sig
  type t = private int [@@deriving equal, compare, sexp, hash]

  include Comparable.S with type t := t

  val zero : t
  val succ : t -> t
end = struct
  module T = struct
    type t = int [@@deriving equal, compare, sexp, hash]
  end

  include T
  include Comparable.Make (T)

  let zero = 0
  let succ t = t + 1
end

module Node = struct
  type 'a t =
    { id : Identifier.t
    ; level : Level.t
    ; value : 'a
    ; parent : 'a t option
    }

  type 'a opaque_t = 'a t

  let sexp_of_opaque_t _sexp_of_a t = [%sexp (t.id : Identifier.t)]

  let sexp_of_t sexp_of_a { id; level; value; parent } =
    [%sexp { id : Identifier.t; level : Level.t; value : a; parent : a opaque_t option }]
  ;;
end

type 'a t = T of 'a Node.t [@@unboxed] [@@deriving sexp_of]

let root (T root) = root

let create ~id_source value =
  T { id = Identifier.create id_source; parent = None; level = Level.zero; value }
;;

let create_node ~id_source ~(parent : 'a Node.t) value : 'a Node.t =
  let level = Level.succ parent.level in
  { id = Identifier.create id_source; parent = Some parent; level; value }
;;

let rec nearest_common_ancestor (t1 : 'a Node.t) (t2 : 'a Node.t) =
  [%log.global.debug
    "nearest common ancestor" (t1.id : Identifier.t) (t2.id : Identifier.t)];
  if Identifier.(t1.id = t2.id)
  then t1
  else if Level.(t1.level < t2.level)
  then nearest_common_ancestor t1 (Option.value_exn ~here:[%here] t2.parent)
  else if Level.(t1.level > t2.level)
  then nearest_common_ancestor (Option.value_exn ~here:[%here] t1.parent) t2
  else (
    assert (Level.(t1.level = t2.level && t1.level > zero));
    nearest_common_ancestor
      (Option.value_exn ~here:[%here] t1.parent)
      (Option.value_exn ~here:[%here] t2.parent))
;;

let is_ancestor (t : 'a Node.t) ~(of_ : 'a Node.t) =
  Identifier.(t.id = (nearest_common_ancestor t of_).id)
;;

let compare_node_by_level (n1 : 'a Node.t) (n2 : 'a Node.t) =
  Level.compare n1.level n2.level
[@@inline]
;;

module With_dirty = struct
  module Tree_node = Node

  module Node = struct
    type 'a desc =
      { value : 'a
      ; mutable dirty : 'a dirty option
      }

    and 'a dirty = { children : (Identifier.t, 'a t) Hashtbl.t }
    and 'a t = 'a desc Tree_node.t
    and 'a opaque_t = 'a desc Tree_node.opaque_t [@@deriving sexp_of]

    let create_desc value = { value; dirty = None }
    let value (t : 'a t) = t.value.value [@@inline]
  end

  module Dirty = struct
    type 'a t = 'a Node.dirty = { children : (Identifier.t, 'a Node.t) Hashtbl.t }
    [@@deriving sexp_of]

    let create () = { children = Hashtbl.create (module Identifier) }
    let is_empty t = Hashtbl.is_empty t.children
    let add_child t (child : 'a Node.t) = Hashtbl.set t.children ~key:child.id ~data:child
    let filter_inplace t ~f = Hashtbl.filter_inplace t.children ~f
    let remove_child t (child : 'a Node.t) = Hashtbl.remove t.children child.id

    let loop_children t ~f =
      let rec loop () =
        [%log.global.debug "loop_children" (Hashtbl.keys t.children : Identifier.t list)];
        match Hashtbl.choose t.children with
        | None -> ()
        | Some (id, child) ->
          [%log.global.debug "loop_children: processing child" (id : Identifier.t)];
          f child;
          loop ()
      in
      loop ()
    ;;
  end

  type nonrec 'a t =
    { root : 'a Node.desc t
    ; dirty_roots : 'a Node.dirty
    }
  [@@deriving sexp_of]

  let root { root = T root; _ } = root [@@inline]
  let is_empty { dirty_roots; _ } = Dirty.is_empty dirty_roots [@@inline]

  let create ~id_source it =
    { root = create ~id_source (Node.create_desc it); dirty_roots = Dirty.create () }
  ;;

  let create_node ~id_source ~parent it =
    create_node ~id_source ~parent (Node.create_desc it)
  ;;

  let rec find_closest_dirty_ancestor t (node : 'a Node.t) =
    match node.parent with
    | None -> t.dirty_roots
    | Some parent ->
      (match parent.value.dirty with
       | Some dirty -> dirty
       | None -> find_closest_dirty_ancestor t parent)
  ;;

  let mark_dirty t (node : 'a Node.t) =
    match node.value.dirty with
    | Some _ -> ()
    | None ->
      let dirty = Dirty.create () in
      node.value.dirty <- Some dirty;
      let anc_dirty = find_closest_dirty_ancestor t node in
      Dirty.filter_inplace anc_dirty ~f:(fun imm_desc ->
        if is_ancestor node ~of_:imm_desc
        then (
          assert (Identifier.(imm_desc.id <> node.id));
          Dirty.add_child dirty imm_desc;
          false)
        else true);
      Dirty.add_child anc_dirty node
  ;;

  let drain_dirty t (node : 'a Node.t) ~before ~f ~after =
    let rec loop_node (node : 'a Node.t) =
      before ();
      match node.value.dirty with
      | None -> ()
      | Some dirty ->
        Dirty.loop_children dirty ~f:loop_node;
        f node;
        assert (Option.is_some node.value.dirty);
        (* If there is no-more remaining work at [node], 
           clear it. Otherwise try again. *)
        if Hashtbl.is_empty dirty.children
        then (
          [%log.global.debug "Node is clean, clearing it :)"];
          let anc_dirty = find_closest_dirty_ancestor t node in
          Dirty.remove_child anc_dirty node;
          node.value.dirty <- None)
        else loop_node node;
        after ()
    in
    loop_node node
  ;;

  let drain_dirty_roots t ~before ~f ~after =
    before ();
    Dirty.loop_children t.dirty_roots ~f:(drain_dirty t ~before ~f ~after)
  ;;
end
