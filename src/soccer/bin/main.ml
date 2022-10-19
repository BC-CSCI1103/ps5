(* file: main.ml
   author: Bob Muller

   CSCI 1103 Computer Science 1 Honors

   This is project for displaying assists in NCAA Women's Soccer.
   The input data gives assists for individual players in a file
   called assists.csv (comma separated values).  The histogram
   depicts the top five total assists -per school-.

   Usage:
   > cd src/
   > dune exec bin/main.exe assists.csv
*)
let displayWidth = 1200.
let displayHeight = 600.

type player = { rank : int
              ; name : string
              ; team : string
              ; cls : string
              ; pos: string
              ; games : int
              ; assists : int
              ; pergame : float
              }

let formatPlayer {rank; name; team; cls; pos; games; assists; pergame} =
  Lib.fmt
   "{rank=%d; name=%s; team=%s; class=%s; pos=%s; games=%d; assists=%d; pergame=%4.2f}"
   rank name team cls pos games assists pergame

type model = player list

let initialModel = []

type school = { name : string
              ; assists : int
              }

(* formatSchool : school -> string *)
let formatSchool {name; assists} =
  Lib.fmt "{name = %s; assists = %d}" name assists

(* printSchool : school -> unit *)
let printSchool school = Lib.pfmt "%s\n" (formatSchool school)

(* printSchools : school list -> unit *)
let printSchools schools = List.iter printSchool schools

(* toNumber : string -> (string -> 'a) -> 'a *)
let toNumber string s2Num =
  let chars = List.filter (fun c -> c <> '\"' ) (Lib.explode string)
  in
  s2Num (Lib.implode chars)

(* removeQuotes : string -> string *)
let removeQuotes line =
  Lib.implode (List.filter (fun c -> c <> '"') (Lib.explode line))

(* processInputLine : string -> player *)
let processInputLine line =
  let line = removeQuotes line in
  let get = List.nth in
  let fields = String.split_on_char ',' line in
  let (r, n, t, c) = (get fields 0, get fields 1, get fields 2, get fields 3) in
  let (p, g, a, pg) = (get fields 4, get fields 5, get fields 6, get fields 7)
  in
  { rank = toNumber r int_of_string
  ; name = n
  ; team = t
  ; cls = c
  ; pos = p
  ; games = toNumber g int_of_string
  ; assists = toNumber a int_of_string
  ; pergame = toNumber pg float_of_string
  }

(* readPlayers : string -> player list *)
let readPlayers filename =
  let inch = open_in filename in
  let _ = input_line inch in  (* Discard first line with column titles. *)
  let rec repeat players =
    (try
       let playerLine = input_line inch in
       let player = processInputLine playerLine
       in
       repeat (player :: players)
     with
       End_of_file -> ( close_in inch
                      ; players
                      ))
  in
  repeat []

(* view : model -> Image.t *)
let view model = (* YOUR CODE HERE *)
  Image.text "CSCI 1103 -- First Midterm Take-Home Exam" ~size:55. Color.maroon

(* finished : model -> bool *)
let finished _ = true

(* go : unit -> unit *)
let go () =
  let model = readPlayers (Sys.argv.(1))
  in
  Animate.start model
    ~name: "NCAA Women's Soccer 2019 - Top 5 Team Assists"
    ~width: displayWidth
    ~height: displayHeight
    ~stopWhen: finished
    ~viewLast: view

let _ =
  let _ = Random.self_init ()
  in
  go ()
