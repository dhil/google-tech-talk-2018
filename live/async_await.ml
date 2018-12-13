(* The async/await idiom using effect handlers. *)

let _TODO = failwith "TODO"

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
    let rec schedule : 'a. 'a future -> (unit -> 'a) -> unit =
      fun future main ->
      match main () with
      | effect Suspend task ->
         (* Suspend the current [task]. *)
         _TODO
         (* Resume the next task. *)
        _TODO
      | effect (Async task) parent ->
         (* Create a new [future] for the new [task]. *)
         _TODO;
         (* Suspend the [parent] task. *)
         _TODO;
         (* Run the new [task]. *)
         _TODO
      | effect (Await future) listener ->
         (* Check whether the [future] has already been completed --
            otherwise add [listener] to the listeners' list. *)
         _TODO
      | result ->
         (* A task has completed with [result]. *)
         (* Notify each listener of the [result]. *)
         _TODO;
         (* Complete the [future]. *)
         _TODO;
         (* Run the next task. *)
         _TODO;
    in
    (* Start by scheduling the [main] task with an empty listeners'
       list. *)
    schedule (ref (Listeners [])) main
end

let example () =
  Random.self_init ();
  let open Async in
  let task name () =
    Printf.printf "[%s] Starting\n%!" name;
    let v = Random.int 100 in
    let delay = Random.float 2.0 in
    wait delay; (* Simulates latency. *)
    Printf.printf "[%s] Ending with %d\n%!" name v;
    v
  in
  let main () =
    let a = async (task "a") in
    let b = async (task "b") in
    let c = async (fun () -> await a + await b) in
    Printf.printf "Sum is %d\n" (await c);
    assert (await a + await b = await c)
  in
  run main

let _ = example ()
