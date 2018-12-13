(* Generators and iterators as effect handlers. *)

(* Predicate that tests whether a given number is a perfect number. *)
let is_perfect n =
  if n mod 2 = 1 || n = 1 then false
  else
    let rec loop i sum n =
      if i * i > n then sum
      else if n mod i = 0 then
        loop (i+1) (sum + i + n / i) n
      else
        loop (i+1) sum n
    in
    let sum = loop 2 1 n in
    sum = n

(* Yield is an abstract operation. *)
(* For simplicity we fix yield to work over integers. *)
effect Yield : int -> unit
let yield e = perform (Yield e)

(** Stream operations. **)
(* Takes up to [n] elements from a given [stream]. *)
let take n stream =
  if n = 0 then ()
  else
    let i = ref 0 in
    match stream () with
    | () -> ()
    | effect (Yield e) k ->
       incr i;       (* increments [i] by one. *)
       yield e;      (* forwards the element [e]. *)
       if !i < n then
         continue k () (* requests the next element from the [stream]. *)

(* Filters a given [stream] according to a particular
   [predicate]. *)
let where predicate stream =
  match stream () with
  | () -> ()
  | effect (Yield e) k when predicate e ->
     yield e; (* forwards the element. *)
     continue k ()
  | effect (Yield _) k ->
     continue k ()

(* An iterator. Iterates a given [stream] and applies [f] to each
   element along with its index in the stream. *)
let iteri f stream =
  let i = ref 0 in
  match stream () with
  | () -> ()
  | effect (Yield e) k ->
     f !i e; incr i; continue k ()

(* Examples *)
(* An "infinite" stream of naturals. *)
let naturals () =
  let i = ref 0 in
  while true do
    yield !i;
    incr i
  done

(* Filters out odd numbers. *)
let even stream =
  let predicate e = e mod 2 = 0 in
  where predicate stream

(* Filters out non-perfect numbers. *)
let perfect stream =
  where is_perfect stream

(* Prints the first four perfect numbers. *)
let example () =
  let first_4_perfects () =
    take 4 (fun () ->
        perfect (fun () ->
            even naturals))
  in
  iteri (fun _ n -> Printf.printf "%d %!" n) first_4_perfects
