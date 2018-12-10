(* PI estimation using the Monte Carlo method. This example is adapted
   from https://www.dartlang.org/. *)

(* A 2D point abstraction. *)
module Point = struct
  type t = float * float

  let make x y = (x,y)

  let is_inside_unit_circle (x,y)
    = x *. x +. y *. y <= 1.
end

(* A stream abstraction. *)
module Stream(E : sig type t end) = struct
  type t = unit -> unit

  (* Yields elements of type [E.t]. *)
  effect Yield : E.t -> unit
  let yield e = perform (Yield e)

  (* Takes up to [n] elements from a given [stream]. *)
  let take n (stream : t) =
    let i = ref 0 in
    match stream () with
    | () -> ()
    | effect (Yield e) k when !i < n->
       incr i;       (* increments [i] by one. *)
       yield e;      (* forwards the element [e]. *)
       continue k () (* requests the next element from the [stream]. *)
    | effect (Yield _) _ -> ()

  (* Filters a given [stream] according to a particular
     [predicate]. *)
  let where predicate (stream : t) =
    match stream () with
    | () -> ()
    | effect (Yield e) k when predicate e ->
       yield e; continue k ()
    | effect (Yield _) k ->
       continue k ()

  (* Computes the length of a given [stream]. *)
  let length (stream : t) =
    let n = ref 0 in
    match stream () with
    | () -> !n
    | effect (Yield _) k ->
       incr n;       (* increments [n] by one. *)
       continue k () (* requests the next element from the [stream]. *)

  (* Iterates a given [stream] and applies [f] to each element along
     with its index in the stream. *)
  let iteri f stream =
    let i = ref 0 in
    match stream () with
    | () -> ()
    | effect (Yield e) k ->
       f !i e; incr i; continue k ()
end

(* Stream instantiations. *)
module PointStream  = Stream(struct type t = Point.t end)
module DoubleStream = Stream(struct type t = float end)

(* Generates a random stream of [Point]s. *)
let generate_random ?seed () =
  (match seed with
  | None -> Random.self_init ()
  | Some seed -> Random.init seed);
  while true do
    let x = Random.float 1.0 in
    let y = Random.float 1.0 in
    let p = Point.make x y in
    PointStream.yield p
  done

(* Generates a stream of increasingly accurate estimates of π. *)
let compute_pi ?(batch=100000) () =
  let total = ref 0 in
  let count = ref 0 in
  while true do
    let num_inside =
      let open PointStream in
      let points = (fun () -> take batch generate_random) in
      let inside = (fun () -> where Point.is_inside_unit_circle points) in
      length inside
    in
    total := batch + !total;
    count := num_inside + !count;
    let ratio = (float_of_int !count) /. (float_of_int !total) in
    (* Area of a circle is A = π⋅r², therefore π = A/r².
     * So, when given random points with x ∈ <0,1>,
     * y ∈ <0,1>, the ratio of those inside a unit circle
     * should approach π / 4. Therefore, the value of π
     * should be: *)
     DoubleStream.yield (ratio *. 4.0)
  done

let main () =
  Printf.printf "Compute π using the Monte Carlo method.\n%!";
  DoubleStream.iteri
    (fun i estimate -> Printf.printf "%3d. π ≅ %f\n%!" (i+1) estimate)
    (fun () -> DoubleStream.take 500 compute_pi);
