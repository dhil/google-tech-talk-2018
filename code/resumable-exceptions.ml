(* A simple and robust calculator using resumable exceptions. *)

(* Override the definition of string to integer conversion to throw
   [Conversion_failure] when conversion fails. *)
exception Conversion_failure of string
let int_of_string str =
  match int_of_string str with
  | success -> success
  | exception Failure _ -> raise (Conversion_failure str)

(* Attempts to read and convert strings from standard input to
   integers.  *)
let rec sum_up result =
  match
    let input = input_line stdin in
    int_of_string input
  with
  | number -> sum_up (number + result)
  | exception (Conversion_failure str) ->
      failwith (Printf.sprintf "Conversion failure '%s'" str)
  | exception End_of_file -> result

let _ =
  let sum = sum_up 0 in
  Printf.printf "Sum is %d\n" sum
