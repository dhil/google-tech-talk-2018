(* A modular implementation of the classic "guess the number"
   game. Most probably programmed a variation of this program during
   their freshman year. *)

let _TODO () = failwith "TODO"

(* Reads an integer from the "environment". *)
let read_int () = int_of_string (input_line stdin)

(* Any guess is either [Correct], too [High], or [too] low. *)
type answer = Correct | High | Low

(* The core routine of the game. We make [Guess]ing an abstract
   operation such that we can interpret it however we like. *)
effect Guess : int -> answer
let rec guess () =
  (* Ask for input. *)
  Printf.printf "Take a guess> %!";
  let number = read_int () in
  (* Make the guess. *)
  match perform (Guess number) with
  | Correct -> Printf.printf "Correct!!\n%!"
  | High    -> Printf.printf "Your guess is too high.\n%!"; guess ()
  | Low     -> Printf.printf "Your guess is too low.\n%!"; guess ()

(* A handler for [Guess] amounts to implementing a validator. *)
let my_secret secret game = _TODO ()

(* Starts an ordinary game. *)
let play () =
  my_secret 10 guess








(* This handler mocks the input data. *)
(* let mock_input (guesses : int list) game =
 *   let rem_guesses = ref guesses in
 *   match game () with
 *   | value -> value
 *   | effect Read k ->
 *      match !rem_guesses with
 *      | [] -> ()
 *      | guess :: guesses ->
 *         Printf.printf "%d\n%!" guess; (\* typing in the number. *\)
 *         rem_guesses := guesses;
 *         continue k guess *)



(* Starts a ordinary game with input handled by an effect handler. *)
(* let play_input () =
 *   input (fun () -> my_secret 10 guess) *)

(* Starts a "mocked" game. *)
(* let play_mock () =
 *   mock_input [7;14;10] (fun () -> my_secret 10 guess) *)









(* This handler accumulates a history of guesses made. *)
(* let history game =
 *   let guesses : (int * answer) list ref = ref [] in
 *   _TODO () *)

(* let play_with_history () =
 *   input
 *     (fun () ->
 *       my_secret 10 (fun () -> history guess)) *)
