(* Module trace.ml *)
effect Trace : unit
let trace f =
  match f (fun () -> perform Trace) with
  | result -> result
  | effect Trace k -> print_endline "Called"; continue k ()

(* Module other.ml *)
let f g =
  match g () with
  |  _ -> ()
  | effect Trace _ -> ()

let _ = trace f (* prints nothing. *)
