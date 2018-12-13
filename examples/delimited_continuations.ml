(* An encoding of shift/reset control operators using effect handlers. *)

(* Shift is a higher order operation. *)
(* For simplicity we fix the operator to build lists of integers. *)
effect Shift : ((unit -> int list) -> int list) -> unit
let shift x = perform (Shift x)

(* Reset is a handler. *)
let reset g =
  match g () with
  | value -> value
  | effect (Shift f) k -> f (fun x -> continue k x)

(* Yield defined in terms of [shift]. *)
let yield x =
  shift (fun k -> x :: k ())

(* Builds the list [1,2,3]. *)
let example () =
  reset (fun () ->
      yield 1;
      yield 2;
      yield 3;
      [])
