(* CSCI 1103/2103: Computer Science 1 Honors

   Immutable binary search trees and a simplified form of immutable
   Left-leaning Red/Black Trees with integer keys, no values. The
   algorithm here is based on the mutable version of LL RB trees in
   Sedgewick & Wayne's Algorithms book.
*)
type key = int

(** Immutable Binary Search Trees ****************************************)

type 'a bst = Empty
            | Node of { key   : key
                      ; value : 'a
                      ; left  : 'a bst
                      ; right : 'a bst
                      }

(* putBST : key -> value -> 'a bst -> 'a bst *)
let rec putBST newKey newValue diction =
  match diction with
  | Empty -> Node {key=newKey; value=newValue; left=Empty; right=Empty}
  | Node {key; value; left; right} ->
    (match compare newKey key with
     |  0 -> Node {key; value = newValue; left; right}
     | -1 -> Node {key; value; left = putBST newKey newValue left; right}
     |  1 -> Node {key; value; left; right = putBST newKey newValue right}
     |  _ -> failwith "This can't happen.")

(* findBST : key -> 'a bst -> 'a *)
let rec findBST searchKey diction =
  match diction with
  | Empty -> failwith "key not found"
  | Node {key; value; left; right} ->
    (match compare searchKey key with
       |  0 -> value
       | -1 -> findBST searchKey left
       |  1 -> findBST searchKey right
       |  _ -> failwith "This can't happen.")

(* isBST : 'a bst -> bool *)
let rec isBST tree =
  failwith "isBST isn't implemented."       (* YOUR CODE HERE *)

(** Immutable Left-leaning Red/Black Trees ********************************)

type color = Red | Black

let formatColor color =
  match color with
  | Red   -> "Red"
  | Black -> "Black"

(* NB: We're leaving out the value field here. *)
type rbt = Empty
         | Node of { color : color
                   ; key   : key
                   ; left  : rbt
                   ; right : rbt
                   }

(* format : rbt -> string *)
let rec format tree =
  match tree with
  | Empty -> ""
  | Node{color; key; left; right} ->
    let clrs = formatColor color in
    let lefts  = format left in
    let rights = format right
    in
    Lib.fmt "Node{color=%s; key=%d; left=%s; right=%s}" clrs key lefts rights

(* isRed : rbt -> bool *)
let isRed tree =
  match tree with
  | Node {color=Red} -> true
  | _ -> false

(* find : key -> rbt -> bool *)
let rec find searchKey tree =
  match tree with
  | Empty -> false
  | Node {key; left; right} ->
    match compare searchKey key with
    |  0 -> true      (* Normally would return value of key here. *)
    | -1 -> find searchKey left
    |  1 -> find searchKey right
    |  _ -> failwith "This can't happen"

(* isLLRBT : rbt -> bool *)
let rec isLLRBT tree =
  failwith "isLLRBT isn't implemented."       (* YOUR CODE HERE *)

(* put : key -> rbt -> rbt *)
let put key tree =
  failwith "put isn't implemented"            (* YOUR CODE HERE *)

let test () =
  let t0 = put 10 Empty in
  let t1 = put 20 t0
  in
  put 15 t1
