(* It's easy to introduce memory leaks in the presence of resources. *)

effect Choose : bool

let take_while predicate file =
  let fh = open_in file in
  let rec take acc =
    match
      let line = input_line fh in
      if predicate line then take (line :: acc)
      else (close_in fh; acc)
    with
    | result -> result
    | exception End_of_file -> close_in fh; acc
  in
  take []


effect Abort : 'a

let _ =
  let predicate _ = perform Abort in
  match take_while predicate "fruits.dat" with
  | result -> result
  | effect Abort _ -> [] (* leaks. *)
