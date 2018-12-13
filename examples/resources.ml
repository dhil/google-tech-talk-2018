(* It's easy to introduce memory leaks in the presence of resources. *)

let take_while predicate file =
  let fh = open_in file in
  let rec take acc =
    try
      let line = input_line fh in
      if predicate line then take (line :: acc)
      else acc
    with
    | End_of_file -> acc
  in
  let lines = take [] in
  close_in fh; lines


effect Abort : 'a
let leaks ()  =
  let predicate _ = perform Abort in
  match take_while predicate "fruits.dat" with
  | result -> result
  | effect Abort _ -> [] (* leaks. *)

(* Multi-shot handlers do not interact well with resources either. *)
effect Choose : bool
let bad_descriptor () =
  let predicate _ = perform Choose in
  match take_while predicate "fruits.dat" with
  | result -> [result]
  | effect Choose k ->
     let k' = Obj.clone_continuation k in
     continue k true @ continue k' false
