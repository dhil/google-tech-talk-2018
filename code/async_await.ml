(* The async/await idiom using effect handlers. *)

module type ASYNC = sig
  type 'a future

  val await : 'a future -> 'a
  val async : (unit -> 'a) -> 'a future
  val suspend : unit -> unit
  val wait : float -> unit

  val run : (unit -> 'a) -> unit
end

module Async : ASYNC = struct
  (* A [future] is either [Done] or has a list of pending
     [Listeners]. *)
  type 'a _future =
    | Done of 'a
    | Listeners of ('a, unit) continuation list

  (* Actually, we implement a [future] as a pointer to a [_future]
     such that we can update a future object in-place. *)
  type 'a future = 'a _future ref

  (* Starts an async computation. *)
  effect Async : (unit -> 'a) -> 'a future
  let async f = perform (Async f)

  (* Awaits a future. *)
  effect Await : 'a future -> 'a
  let await f = perform (Await f)

 (* Suspends the current task. Thereby allowing another task to run. *)
  effect Suspend : unit
  let suspend () = perform Suspend

  (* Waits at least [duration] before continuing the current task. *)
  let wait duration =
    let rec loop start duration =
      let elapsed = Unix.gettimeofday () -. start in
      if elapsed >= duration then ()
      else (suspend (); loop start duration)
    in
    loop (Unix.gettimeofday ()) duration

  (* A runtime queue for scheduling tasks. *)
  let run_queue = Queue.create ()
  let enqueue thread = Queue.push thread run_queue
  let dequeue () =
    if Queue.is_empty run_queue then ()
    else let next = Queue.pop run_queue in
         next ()

  (* Runs [main] in an asynchronous context. *)
  let run main =
    let rec fork : 'a. 'a future -> (unit -> 'a) -> unit =
      fun future main ->
      match main () with
      | result ->
         (* A task has completed with [result]. *)
         let listeners =
           match !future with
           | Listeners ks -> ks
           | _ -> failwith "Impossible!"
         in
         (* Notify each listener of the result. *)
         List.iter
           (fun listener ->
             enqueue (fun () -> continue listener result)) listeners;
         (* Complete the [future]. *)
         future := Done result;
         (* Run the next task. *)
         dequeue ()
      | effect (Async task) parent ->
         (* Create a new [future] for the new [task]. *)
         let future = ref (Listeners []) in
         (* Suspend the [parent] task. *)
         enqueue (fun () -> continue parent future);
         (* Run the new [task]. *)
         fork future task
      | effect (Await future) listener ->
         (* Check whether the [future] has already been completed --
            otherwise add [listener] to the listeners' list. *)
         (match !future with
          | Done v -> continue listener v
          | Listeners ls ->
             future := Listeners (listener :: ls);
             dequeue ())
      | effect Suspend task ->
         (* Suspend the current [task]. *)
         enqueue (fun () -> continue task ());
         (* Resume the next task. *)
         dequeue ()
    in
    (* Start by forking [main] with an empty listeners' list. *)
    fork (ref (Listeners [])) main
end

let example () =
  let open Async in
  let main () =
    let hello_task =
      async (fun () -> wait 1.5; print_endline "Hello")
    in
    let world_task =
      async (fun () -> wait 0.5; print_endline "World")
    in
    await hello_task;
    await world_task
  in
  run main

let _ = example ()
