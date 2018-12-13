(* Solution to the same fringe problem. Challenge: decide whether any
   two binary trees have the same fringe using at most linear time and
   constant space. *)

(* For simplicity we fix trees over integers. *)
type tree = Leaf of int
          | Node of tree * tree

(* Calling [Yield] suspends the current computation, and hands over
   control to its iterator along with the provided integer value. *)
effect Yield : int -> unit
let yield x = perform (Yield x)

(* Generator: given a tree, this routine generates a stream of
   integers (the leaves). *)
let rec walk_tree = function
  | Leaf value -> yield value
  | Node (lhs, rhs) ->
     walk_tree lhs;
     walk_tree rhs

(* To walk two trees simultaneously we need to use a little trick:
   reification. Effects can be reified as concrete data. *)

(* Reification of [Yield]. *)
type reified = Done
             | Yielded of int * (unit, reified) continuation

(* Evaluates [f] until [Yield] occurs, and then reifies the [Yield]
   along with its continuation. *)
let reify f () =
  match f () with
  | _ -> Done
  | effect (Yield value) k ->
     Yielded (value, k)

(* Given two reifying tree computations, this routine decides whether
   the underlying trees have the same fringe. *)
let rec same_fringe left right =
  match
    left  (),
    right ()
  with
  | Done, Done -> true
  | Done, _ | _, Done -> false
  | Yielded (value1, k1), Yielded (value2, k2) ->
     value1 = value2
     && same_fringe
          (fun () -> continue k1 ())
          (fun () -> continue k2 ())

(* Shadows the above definition; wraps [left] and [right] using
   [reify]. *)
let same_fringe left right =
  same_fringe
    (reify (fun () -> walk_tree left))
    (reify (fun () -> walk_tree right))
