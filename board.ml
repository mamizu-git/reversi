open Int64
open Color
open Const

let init_bitboard () = 
  let myboard = 0x0000000810000000L in
  let opboard = 0x0000001008000000L in
  (myboard, opboard)

let popcount (x : int64) : int =
  let a = 0x5555555555555555L in
  let b = 0x3333333333333333L in
  let c = 0x0f0f0f0f0f0f0f0fL in
  let d = 0x0101010101010101L in
  let x = sub x (logand (shift_right_logical x 1) a) in
  let x = add (logand x b) (logand (shift_right_logical x 2) b) in
  let x = logand (add x (shift_right_logical x 4)) c in
  to_int (shift_right_logical (mul x d) 56)

let get_color_bb board i j =
  let (myboard, opboard) = board in
  if (logand (shift_right_logical myboard ((j-1)*8+(i-1))) 0x1L) = 0x1L then
    !turncolor
  else if (logand (shift_right_logical opboard ((j-1)*8+(i-1))) 0x1L) = 0x1L then
    opposite_color !turncolor
  else 
    none

let is_not_empty board i j =
  (logand (shift_right_logical board ((j-1)*8+(i-1))) 0x1L) = 0x1L

let isinboard i j = 
  (0 < i) && (i < 9) && (0 < j) && (j < 9)

let emptycount board = 
  let (myboard, opboard) = board in
  64 - popcount myboard - popcount opboard

let valid_move board : int64 =
  let (myboard, opboard) = board in
  let horizontalWatchBoard = logand 0x7e7e7e7e7e7e7e7eL opboard in
	let verticalWatchBoard = logand 0x00FFFFFFFFFFFF00L opboard in
	let allSideWatchBoard = logand 0x007e7e7e7e7e7e00L opboard in
	let blankBoard = lognot (logor myboard opboard) in
	let tmp = ref 0x0L in
	let res = ref 0x0L in

  (* left *)
  tmp := logand horizontalWatchBoard (shift_left myboard 1);
  tmp := logor !tmp (logand horizontalWatchBoard (shift_left !tmp 1));
  tmp := logor !tmp (logand horizontalWatchBoard (shift_left !tmp 1));
  tmp := logor !tmp (logand horizontalWatchBoard (shift_left !tmp 1));
  tmp := logor !tmp (logand horizontalWatchBoard (shift_left !tmp 1));
  tmp := logor !tmp (logand horizontalWatchBoard (shift_left !tmp 1));
  res := logand blankBoard (shift_left !tmp 1);

  (* right *)
  tmp := logand horizontalWatchBoard (shift_right_logical myboard 1);
  tmp := logor !tmp (logand horizontalWatchBoard (shift_right_logical !tmp 1));
  tmp := logor !tmp (logand horizontalWatchBoard (shift_right_logical !tmp 1));
  tmp := logor !tmp (logand horizontalWatchBoard (shift_right_logical !tmp 1));
  tmp := logor !tmp (logand horizontalWatchBoard (shift_right_logical !tmp 1));
  tmp := logor !tmp (logand horizontalWatchBoard (shift_right_logical !tmp 1));
  res := logor !res (logand blankBoard (shift_right_logical !tmp 1));
  
  (* up *)
  tmp := logand verticalWatchBoard (shift_left myboard 8);
  tmp := logor !tmp (logand verticalWatchBoard (shift_left !tmp 8));
  tmp := logor !tmp (logand verticalWatchBoard (shift_left !tmp 8));
  tmp := logor !tmp (logand verticalWatchBoard (shift_left !tmp 8));
  tmp := logor !tmp (logand verticalWatchBoard (shift_left !tmp 8));
  tmp := logor !tmp (logand verticalWatchBoard (shift_left !tmp 8));
  res := logor !res (logand blankBoard (shift_left !tmp 8));

  (* down *)
  tmp := logand verticalWatchBoard (shift_right_logical myboard 8);
  tmp := logor !tmp (logand verticalWatchBoard (shift_right_logical !tmp 8));
  tmp := logor !tmp (logand verticalWatchBoard (shift_right_logical !tmp 8));
  tmp := logor !tmp (logand verticalWatchBoard (shift_right_logical !tmp 8));
  tmp := logor !tmp (logand verticalWatchBoard (shift_right_logical !tmp 8));
  tmp := logor !tmp (logand verticalWatchBoard (shift_right_logical !tmp 8));
  res := logor !res (logand blankBoard (shift_right_logical !tmp 8));

  (* rightup *)
  tmp := logand allSideWatchBoard (shift_left myboard 7);
  tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 7));
  tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 7));
  tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 7));
  tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 7));
  tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 7));
  res := logor !res (logand blankBoard (shift_left !tmp 7));

  (* leftup *)
  tmp := logand allSideWatchBoard (shift_left myboard 9);
  tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 9));
  tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 9));
  tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 9));
  tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 9));
  tmp := logor !tmp (logand allSideWatchBoard (shift_left !tmp 9));
  res := logor !res (logand blankBoard (shift_left !tmp 9));

  (* rightdown *)
  tmp := logand allSideWatchBoard (shift_right_logical myboard 9);
  tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 9));
  tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 9));
  tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 9));
  tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 9));
  tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 9));
  res := logor !res (logand blankBoard (shift_right_logical !tmp 9));

  (* leftdown *)
  tmp := logand allSideWatchBoard (shift_right_logical myboard 7);
  tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 7));
  tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 7));
  tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 7));
  tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 7));
  tmp := logor !tmp (logand allSideWatchBoard (shift_right_logical !tmp 7));
  res := logor !res (logand blankBoard (shift_right_logical !tmp 7));

  !res

let valid_move_list board : (int*int) list =
  let res = ref [] in
  let moves = valid_move board in
  for i=1 to 8 do
    for j=1 to 8 do
      if (logand (shift_right_logical moves ((j-1)*8+(i-1))) 0x1L) = 0x1L then
        res := (i,j) :: !res
      else ()
    done
  done;
  !res

let transfer i j dir = 
  match dir with 
  | 1 -> (i+1,j)
  | 2 -> (i+1,j+1) 
  | 3 -> (i,j+1)
  | 4 -> (i-1,j+1)
  | 5 -> (i-1,j)
  | 6 -> (i-1,j-1)
  | 7 -> (i,j-1)
  | 8 -> (i+1,j-1)
  | _ -> (i,j)

let flip board i j =
  let (myboard, opboard) = board in
  let res = ref 0x0L in
  res := logor !res (shift_left 0x1L ((j-1)*8+(i-1)));
  let res2 = ref 0x0L in
  let rec flip_sub i j dir =
    let (i,j) = transfer i j dir in
    if (isinboard i j) && (is_not_empty opboard i j) then 
      (res2 := logor !res2 (shift_left 0x1L ((j-1)*8+(i-1)));
      flip_sub i j dir)
    else if (isinboard i j) && (is_not_empty myboard i j) then
      res := logor !res !res2
    else ()
  in
  for dir=1 to 8 do
    flip_sub i j dir;
    res2 := 0x0L
  done;
  let myboard = logor !res myboard in
  let opboard = logand (lognot !res) opboard in
  (myboard, opboard)

let boardchange (board : (int64*int64)) : (int64*int64) = 
  let (myboard, opboard) = board in
  (opboard, myboard)


