open Color
open Command
open Board
open Const
open New
open Eval

let play board : move = 
  let empty = emptycount board in (* *)
  if empty = 60 then
    Mv (4,3)
  else if empty > middle then 
    (* early *)
    get_earlymove board
  else if empty > !last then
    (* middle *)
    (
    (* print_string "Eval: "; print_int (eval_middle2 board);
    print_endline ""; *)
    get_middlemove board depth_middle !middle_limit)
  else if empty = !last || empty = !last - 1 then
    let (myb,opb) = board in
    (last_rest := !remaining_time;
    (* print_string "time: "; print_float !remaining_time; print_endline "";
    print_string "lastdepth: "; print_int empty; print_endline "";
    print_string "CN: "; print_int (popcount (valid_move board) - popcount (valid_move (opb,myb)));
    print_endline "";
    print_string "BP: "; print_int (get_bp bplist board);
    print_endline "";
    print_string "FS: "; print_int (get_fs board);
    print_endline "";
    print_string "Eval: "; print_int (eval_middle2 board);
    print_endline ""; *)
    get_lastmove board empty)
  else 
    (* last *)
    get_lastmove board empty


let print_bitboard board =
  print_endline " |A B C D E F G H ";
  print_endline "-+----------------";
  for j=1 to 8 do
    print_int j; print_string "|";
    for i=1 to 8 do
      print_color (get_color_bb board i j); print_string " "
    done;
    print_endline ""
  done;
  print_endline "  (X: Black,  O: White)"


let report_result board =
  let _ = print_endline "========== Final Result ==========" in
  let bc = popcount (whose_board board !turncolor) in
  let wc = popcount (whose_board board (opposite_color !turncolor)) in
    if bc > wc then
      print_endline "*Black wins!*"
    else if bc < wc then
      print_endline "*White wins!*"
    else
      print_endline "*Even*";
    print_string "Black: "; print_endline (string_of_int bc);
    print_string "White: "; print_endline (string_of_int wc);
    print_bitboard board