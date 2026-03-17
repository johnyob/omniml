open! Import

type job = unit -> unit
and t = { job_queue : job Queue.t } [@@deriving sexp_of]

let create () = { job_queue = Queue.create () }
let t_ref = ref (create ())
let t () = !t_ref
let is_empty t = Queue.is_empty t.job_queue
let enqueue t job = Queue.enqueue t.job_queue job
let enqueue_all t jobs = Queue.enqueue_all t.job_queue jobs

let run t =
  let rec loop () =
    match Queue.dequeue t.job_queue with
    | None -> ()
    | Some job ->
      job ();
      loop ()
  in
  loop ()
;;

let clear t = Queue.clear t.job_queue
