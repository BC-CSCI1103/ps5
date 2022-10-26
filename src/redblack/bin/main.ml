(* CSCI 1103/2103: Computer Science 1 Honors/Functional Programming

   A Simplified form of immutable Left-leaning Red/Black Trees
   with integer keys, no values.  The algorithm here is based on
   the mutable version of LL RB trees in Sedgewick & Wayne's
   Algorithms book.
*)
type key   = int
type color = Red | Black

let formatColor color =
  match color with
  | Red   -> "Red"
  | Black -> "Black"

(** Immutable Binary Search Trees ****************************************

    If we ignore the color field, we have plain immutable BSTs. Taking the
    color field into account, we have immutable left-leaning red/black
    trees.
*)
type 'a rbt = Empty
            | Node of { key   : key
                      ; value : 'a
                      ; color : color
                      ; left  : 'a rbt
                      ; right : 'a rbt
                      }

(* format : 'a rbt -> string *)
let rec format tree =
  match tree with
  | Empty -> ""
  | Node{color; key; left = Empty; right = Empty} ->
    Lib.fmt "Node{color=%s; key=%d}" (formatColor color) key
  | Node{color; key; left = Empty; right} ->
    let rs = format right
    in
    Lib.fmt "Node{color=%s; key=%d; right=%s}" (formatColor color) key rs
  | Node{color; key; left; right = Empty} ->
    let ls = format left
    in
    Lib.fmt "Node{color=%s; key=%d; left=%s}" (formatColor color) key ls
  | Node{color; key; left; right} ->
    let clrs   = formatColor color in
    let lefts  = format left in
    let rights = format right
    in
    Lib.fmt "Node{color=%s; key=%d; left=%s; right=%s}" clrs key lefts rights

(* putBST : key -> value -> 'a rbt -> 'a rbt *)
let rec putBST newKey newValue diction =
  match diction with
  | Empty -> Node { key   = newKey
                  ; value = newValue
                  ; color = Black
                  ; left  = Empty
                  ; right = Empty
                  }
  | Node {key; value; left; right} ->
    (match compare newKey key with
     |  0 -> Node {key; value = newValue; color = Black; left; right}
     | -1 -> Node { key
                  ; value
                  ; color = Black
                  ; left  = putBST newKey newValue left
                  ; right
                  }
     |  1 -> Node { key
                  ; value
                  ; color = Black
                  ; left
                  ; right = putBST newKey newValue right
                  }
     |  _ -> failwith "This can't happen.")

(* findBST : key -> 'a rbt -> 'a *)
let rec findBST searchKey diction =
  match diction with
  | Empty -> failwith "key not found"
  | Node {key; value; left; right} ->
    (match compare searchKey key with
       |  0 -> value
       | -1 -> findBST searchKey left
       |  1 -> findBST searchKey right
       |  _ -> failwith "This can't happen.")

(* isBST : 'a rbt -> bool *)
let rec isBST tree =
  failwith "isBST isn't implemented."    (* YOUR CODE HERE *)

(** Immutable Left-leaning Red/Black Trees ********************************)

(* isLLRBT : 'a rbt -> bool *)
let isLLRBT tree =
  failwith "isLLRBT isn't implemented."       (* YOUR CODE HERE *)

(* find : key -> rbt -> 'a *)
let rec find searchKey tree =
  match tree with
  | Empty -> failwith "key not found"
  | Node {key; value; left; right} ->
    match compare searchKey key with
    |  0 -> value
    | -1 -> find searchKey left
    |  1 -> find searchKey right
    |  _ -> failwith "This can't happen"

(* put : key -> value -> 'a rbt -> 'a rbt

   put handles only the root which should always be Black.
*)
let put key value tree =
  failwith "put isn't implemented"            (* YOUR CODE HERE *)

(**** Testing ***************************************************)

(* makeNode : key -> left -> right -> tree
   default color is Black and value 0.
*)
let makeNode ?(color=Black) key left right =
  Node {color; key; value = 0; left; right}

let t0 = Empty
let t1 = makeNode 20 t0 t0
let t2 = makeNode 10 t1 t0
let t3 = makeNode 30 t1 t0
let t4 = makeNode 40 t3 t0

let testIsBST0 () = not (isBST t2)
let testIsBST1 () = isBST t3
let testIsBST2 () = isBST t4

let () = Lib.pfmt "Testing isBST...\n"
let () = Lib.run_test "TestIsBST0" testIsBST0
let () = Lib.run_test "TestIsBST1" testIsBST1
let () = Lib.run_test "TestIsBST2" testIsBST2

let t5 = makeNode
            40
            (makeNode ~color:Red 30 t0 t0)
            (makeNode 50 t0 t0)
let t6 = (makeNode
            40
            (makeNode
               ~color:Red
               30
               (makeNode ~color:Red 20 t0 t0)
               t0)
            t0)
let t7 = (makeNode
            40
            (makeNode ~color:Red 30 t0 t0)
            (makeNode ~color:Red 50 t0 t0))

let testIsLLRBT0 () = isLLRBT t5
let testIsLLRBT1 () = not (isLLRBT t6)
let testIsLLRBT2 () = not (isLLRBT t7)

let () = Lib.pfmt "Testing isLLRBT0...\n"
let () = Lib.run_test "TestIsRBT0" testIsLLRBT0
let () = Lib.run_test "TestIsRBT1" testIsLLRBT1
let () = Lib.run_test "TestIsRBT2" testIsLLRBT2

let () = Lib.pfmt "Testing put...\n"

let t8 = put 35 0 t5
let t9 = put 50 0 (put 60 0 (put 10 0 t6))
(* keys = [0; 1; ...; 49; 99; 98; ...; 50] *)
let keys = (Lib.range 50) @ (List.rev (List.map ((+) 50) (Lib.range 50)))
let t10 = List.fold_right (fun key tree -> put key 0 tree) keys t0

let testPut0 () = isLLRBT t8
let testPut1 () = isLLRBT t9
let testPut2 () = isLLRBT t10
let () = Lib.run_test "TestPut0" testPut0
let () = Lib.run_test "TestPut1" testPut1
let () = Lib.run_test "TestPut2" testPut2
