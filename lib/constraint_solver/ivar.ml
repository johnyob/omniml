open! Import

module Callback = struct
  module State = struct
    type t =
      | Pending
      | Scheduled
      | Cancelled
    [@@deriving sexp]

    let is_pending t =
      match t with
      | Pending -> true
      | Scheduled | Cancelled -> false
    ;;

    let is_scheduled t =
      match t with
      | Scheduled -> true
      | Pending | Cancelled -> false
    ;;

    let is_cancelled t =
      match t with
      | Cancelled -> true
      | Pending | Scheduled -> false
    ;;
  end

  type 'a t =
    { mutable state : State.t
    ; run : 'a -> unit
    ; cancel : (unit -> unit) option
    }
  [@@deriving sexp_of]

  let create ~run ?cancel () = { state = Pending; run; cancel }
  let is_pending t = State.is_pending t.state
  let is_cancelled t = State.is_cancelled t.state
  let is_scheduled t = State.is_scheduled t.state

  let raise_if_not_pending t ~here =
    if not (is_pending t)
    then
      raise_bug_s
        ~here
        [%message "Ivar.Callback expects pending callback" (t.state : State.t)]
  ;;

  let schedule t ~with_:x ~scheduler =
    raise_if_not_pending t ~here:[%here];
    t.state <- Scheduled;
    Scheduler.enqueue scheduler (fun () -> t.run x)
  ;;

  let cancel t ~scheduler =
    raise_if_not_pending t ~here:[%here];
    match t.cancel with
    | Some cancel ->
      t.state <- Cancelled;
      Scheduler.enqueue scheduler cancel;
      `Ok
    | None -> `Cannot_cancel
  ;;
end

module Cell = struct
  type 'a t =
    | Full of 'a (** [Full x] is a full [Ivar] cell with value [x]. *)
    | Empty of 'a Callback.t Doubly_linked.t
    (** [Empty callbacks] is an empty list of pending [callbacks]. *)
  [@@deriving sexp_of]

  let empty () = Empty (Doubly_linked.create ())

  let merge t1 t2 ~scheduler ~f =
    match t1, t2 with
    | Full x1, Full x2 -> Full (f x1 x2)
    | Full x, Empty callbacks | Empty callbacks, Full x ->
      Doubly_linked.iter callbacks ~f:(Callback.schedule ~with_:x ~scheduler);
      Full x
    | Empty callbacks1, Empty callbacks2 ->
      Doubly_linked.transfer ~src:callbacks2 ~dst:callbacks1;
      Empty callbacks1
  ;;
end

type 'a t = 'a Cell.t Union_find.t [@@deriving sexp_of]
type 'a ivar = 'a t [@@deriving sexp_of]

let create () : 'a t = Union_find.create (Cell.empty ())
let cell (t : 'a t) : 'a Cell.t = Union_find.get t
let set_cell (t : 'a t) (cell : 'a Cell.t) : unit = Union_find.set t cell
let same (t1 : 'a t) (t2 : 'a t) : bool = Union_find.same_class t1 t2

let merge t1 t2 ~scheduler ~f =
  if not (same t1 t2)
  then (
    let cell1 = cell t1
    and cell2 = cell t2 in
    let cell = Cell.merge cell1 cell2 ~scheduler ~f in
    Union_find.union t1 t2;
    set_cell t1 cell)
;;

let fill t x ~scheduler =
  match cell t with
  | Cell.Full y -> `Full y
  | Empty handlers ->
    set_cell t (Full x);
    Doubly_linked.iter handlers ~f:(Callback.schedule ~with_:x ~scheduler);
    `Ok
;;

module Handler = struct
  type 'a t =
    | Scheduled
    (** [Scheduled] is a already scheduled handler. It is an optimization for the 
        fast-path in [upon]. *)
    | Queued of
        { ivar : 'a ivar
        ; callback_elt : 'a Callback.t Doubly_linked.Elt.t
        }
    (** [Queued { ivar; callback_elt }] is queued handler. It may be pending, scheduled, or cancelled. *)
  [@@deriving sexp_of]

  let cancel t ~scheduler =
    match t with
    | Scheduled -> `Already_scheduled
    | Queued { ivar; callback_elt } ->
      let callback = Doubly_linked.Elt.value callback_elt in
      if Callback.is_cancelled callback
      then `Already_cancelled
      else (
        match cell ivar with
        | Full _ ->
          assert (Callback.is_scheduled callback);
          `Already_scheduled
        | Empty callbacks ->
          assert (Callback.is_pending callback);
          (match Callback.cancel callback ~scheduler with
           | `Ok ->
             Doubly_linked.remove callbacks callback_elt;
             `Ok
           | `Cannot_cancel -> `Cannot_cancel))
  ;;
end

let upon t ~run ?cancel ~scheduler () : 'a Handler.t =
  match cell t with
  | Full x ->
    Scheduler.enqueue scheduler (fun () -> run x);
    Scheduled
  | Empty callbacks ->
    let callback = Callback.create ~run ?cancel () in
    let callback_elt = Doubly_linked.insert_last callbacks callback in
    Queued { ivar = t; callback_elt }
;;

let cancel_all t ~scheduler =
  match cell t with
  | Full _ -> ()
  | Empty callbacks ->
    Doubly_linked.filter_inplace callbacks ~f:(fun callback ->
      (* Safety: [callback] must be pending *)
      assert (Callback.is_pending callback);
      match Callback.cancel callback ~scheduler with
      | `Ok -> false
      | `Cannot_cancel -> true)
;;
