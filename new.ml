open Color
open Const
open Command
open Board
open Eval
open Book
open Search
open Int64

let rotate_move move : move =
  match move with
  | Mv (i,j) -> 
    (match !rotate with
    | 0 -> Mv (i,j)
    | 1 -> Mv (j,i)
    | 2 -> Mv (9-i,9-j)
    | 3 -> Mv (9-j,9-i)
    | _ -> move)
  | _ -> move

let turnchange (board : (int64*int64)) : (int64*int64) =
  let (myboard, opboard) = board in
  turncolor := opposite_color !turncolor;
  (opboard, myboard)

let get_hist i j = 
  Char.escaped (Char.chr ((j-1) * 8 + (i-1) + 33))

let num_to_pos x = 
  let i = x mod 8 + 1 in
  let j = x / 8 + 1 in
  (i,j)

let doMove board com =
  match com with
  | GiveUp -> turnchange board
  | Pass -> turnchange board
  | Mv (i,j) ->
    bookhist := !bookhist ^ (get_hist i j);
    let board = flip board i j in
    turnchange board

let rec get_middlemove board depth lim = 
  let ms = ref (sort_last (valid_move_list board) board) in
    let m = ref Pass in
    let d = ref 4 in
    let f = ref 0 in
    let t = ref 0. in
    while !d <= depth_middle && !f = 0 do
      try 
        (* print_int !d; print_endline "";  *)
        let (_,mm) = sc_middle (-100000,Pass) !ms board !d alpha beta 1 lim in 
        m := mm;
        d := !d + 1;
        t := Unix.gettimeofday () -. !start_time
      with
      | Eval.Timeout ->
        f := 1
    done;
    (* print_string "depth: "; print_int depth; print_endline "";
    print_string "middle: ";print_float (Unix.gettimeofday () -. !start_time); print_endline "\n"; *)
    waste_time := !waste_time +. (Unix.gettimeofday () -. !start_time) -. !t;
    if !m = Pass && !ms != [] then
      let k = Random.int (List.length !ms) in
      let (i,j) = List.nth !ms k in
      Mv (i,j)
    else !m



let get_earlymove board = 
  let ms = Hashtbl.find_all joseki !bookhist in
  if ms = [] then
    get_middlemove board depth_middle !middle_limit
  else
    let k = Random.int (List.length ms) in
    let (i,j) = num_to_pos (List.nth ms k) in
    Mv (i,j)

let get_lastmove board depth = 
  try 
    let ms = ref (sort_last (valid_move_list board) board) in
    let (v,m) = ab_last2 (Lose,Pass) !ms board depth in
    (* print_int depth; print_endline "\n"; *)
    (* print_string "last: ";print_float (Unix.gettimeofday () -. !start_time); print_endline "\n"; *)
    (if Unix.gettimeofday () -. !start_time < 1.0 && !last < last_max && (!last = (emptycount board + 1) || !last = emptycount board) then
      last := !last + 1
    else ());
    if m = Pass && !ms != [] then
      (
        (* print_endline "cannot win\n"; start_time := Unix.gettimeofday (); *)
      start_time := Unix.gettimeofday ();
      let (v,m) = ab_last3 (Lose,Pass) !ms board depth in
      if m = Pass && !ms != [] then
        (
          (* print_endline "cannot tie\n"; start_time := Unix.gettimeofday (); *)
        get_middlemove board depth_middle !middle_limit)
      else m)
    else m
  with
  | Eval.Timeout -> 
    (* print_endline "======================================================================================"; *)
    start_time := Unix.gettimeofday ();
    get_middlemove board depth_middle (!middle_limit /. 2.)

let whose_board (board : (int64*int64)) color : int64 =
  let (myboard, opboard) = board in
  if color = black then 
    myboard
  else
    opboard 