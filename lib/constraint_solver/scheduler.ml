open! Import

type job = unit -> unit

and queued_job =
  { sequence : int
  ; job : job
  }

and t =
  { maintenance_jobs : queued_job Queue.t
  ; handler_jobs : queued_job Queue.t
  ; mutable next_sequence : int
  ; mutable running : bool
  ; mutable running_handler : bool
  ; mutable draining_maintenance : bool
  }
[@@deriving sexp_of]

let create () =
  { maintenance_jobs = Queue.create ()
  ; handler_jobs = Queue.create ()
  ; next_sequence = 0
  ; running = false
  ; running_handler = false
  ; draining_maintenance = false
  }
;;

let is_empty t = Queue.is_empty t.maintenance_jobs && Queue.is_empty t.handler_jobs
let is_maintenance_empty t = Queue.is_empty t.maintenance_jobs

let enqueue_job t queue job =
  let queued_job = { sequence = t.next_sequence; job } in
  t.next_sequence <- t.next_sequence + 1;
  Queue.enqueue queue queued_job
;;

let enqueue t job = enqueue_job t t.maintenance_jobs job
let enqueue_all t jobs = List.iter jobs ~f:(enqueue t)
let enqueue_handler t job = enqueue_job t t.handler_jobs job

let drain_maintenance t =
  if not t.draining_maintenance
  then (
    t.draining_maintenance <- true;
    Exn.protect
      ~f:(fun () ->
        let rec loop () =
          match Queue.dequeue t.maintenance_jobs with
          | None -> ()
          | Some { job; _ } ->
            job ();
            loop ()
        in
        loop ())
      ~finally:(fun () -> t.draining_maintenance <- false))
;;

let run t =
  if t.running
  then (
    if
      (* A handler may need deferred instance unifications before forcing
       generalization.  Those jobs are safe to flush, but entering another
       handler here would make handler execution re-entrant. *)
      t.running_handler
    then drain_maintenance t)
  else (
    t.running <- true;
    Exn.protect
      ~f:(fun () ->
        let dequeue_next () =
          match Queue.peek t.maintenance_jobs, Queue.peek t.handler_jobs with
          | None, None -> None
          | Some _, None ->
            Option.map (Queue.dequeue t.maintenance_jobs) ~f:(fun job -> `Maintenance job)
          | None, Some _ ->
            Option.map (Queue.dequeue t.handler_jobs) ~f:(fun job -> `Handler job)
          | Some maintenance, Some handler ->
            if maintenance.sequence < handler.sequence
            then
              Option.map (Queue.dequeue t.maintenance_jobs) ~f:(fun job ->
                `Maintenance job)
            else Option.map (Queue.dequeue t.handler_jobs) ~f:(fun job -> `Handler job)
        in
        let rec loop () =
          match dequeue_next () with
          | None -> ()
          | Some (`Maintenance { job; _ }) ->
            job ();
            loop ()
          | Some (`Handler { job; _ }) ->
            t.running_handler <- true;
            Exn.protect ~f:job ~finally:(fun () -> t.running_handler <- false);
            loop ()
        in
        loop ())
      ~finally:(fun () -> t.running <- false))
;;

let clear t =
  if t.running then invalid_arg "Scheduler.clear: scheduler is running";
  Queue.clear t.maintenance_jobs;
  Queue.clear t.handler_jobs
;;
