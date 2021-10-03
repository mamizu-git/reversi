open Board
open Const
open Command
open Int64

exception Timeout

let eval_last2 board = 
  let (myboard, opboard) = board in
  if popcount myboard > popcount opboard then
    Win
  else if popcount myboard < popcount opboard then
    Lose
  else Tie

let cornercount board = 
  popcount (logand 0x8100000000000081L board)

let get_bp bplist board =
  let res = ref 0 in
  let (myb,opb) = board in
  for i=0 to 7 do
    for j=0 to 7 do
      if (logand (shift_right_logical myb (i*8+j)) 0x1L) = 0x1L then
        res := !res + bplist.(i*8+j)
      else if (logand (shift_right_logical opb (i*8+j)) 0x1L) = 0x1L then
        res := !res - bplist.(i*8+j)
      else ()
    done
  done;
  !res

let get_fs_up myb opb = 
  let rec get_fs_up_sub1 myb n res =
    if (logand (shift_right_logical myb n) 0x1L) = 0x1L && n <= 6 then
      get_fs_up_sub1 myb (n+1) (res+1)
    else 
      res
  in
  let rec get_fs_up_sub2 myb n res =
    if (logand (shift_right_logical myb (7-n)) 0x1L) = 0x1L && n <= 6 then
      get_fs_up_sub2 myb (n+1) (res+1)
    else 
      res
  in
  if (logand 0x00000000000000ffL (logor myb opb)) = 0x00000000000000ffL then
    popcount (logand myb 0x000000000000007eL)
  else 
    let res = ref 0 in
    (if (logand 0x0000000000000001L myb) = 0x1L then
      res := !res + get_fs_up_sub1 myb 1 0
    else ());
    (if (logand 0x0000000000000080L myb) = 0x80L then
      res := !res + get_fs_up_sub2 myb 1 0
    else ());
    !res

let get_fs_down myb opb = 
  let rec get_fs_down_sub1 myb n res =
    if (logand (shift_right_logical myb (n+56)) 0x1L) = 0x1L && n <= 6 then
      get_fs_down_sub1 myb (n+1) (res+1)
    else 
      res
  in
  let rec get_fs_down_sub2 myb n res =
    if (logand (shift_right_logical myb (63-n)) 0x1L) = 0x1L && n <= 6 then
      get_fs_down_sub2 myb (n+1) (res+1)
    else 
      res
  in
  if (logand 0xff00000000000000L (logor myb opb)) = 0xff00000000000000L then
    popcount (logand myb 0x7e00000000000000L)
  else 
    let res = ref 0 in
    (if (logand 0x0100000000000000L myb) = 0x0100000000000000L then
      res := !res + get_fs_down_sub1 myb 1 0
    else ());
    (if (logand 0x8000000000000000L myb) = 0x8000000000000000L then
      res := !res + get_fs_down_sub2 myb 1 0
    else ());
    !res

let get_fs_left myb opb = 
  let rec get_fs_left_sub1 myb n res =
    if (logand (shift_right_logical myb (n*8)) 0x1L) = 0x1L && n <= 6 then
      get_fs_left_sub1 myb (n+1) (res+1)
    else 
      res
  in
  let rec get_fs_left_sub2 myb n res =
    if (logand (shift_right_logical myb (56-n*8)) 0x1L) = 0x1L && n <= 6 then
      get_fs_left_sub2 myb (n+1) (res+1)
    else 
      res
  in
  if (logand 0x0101010101010101L (logor myb opb)) = 0x0101010101010101L then
    popcount (logand myb 0x0001010101010100L)
  else 
    let res = ref 0 in
    (if (logand 0x0000000000000001L myb) = 0x1L then
      res := !res + get_fs_left_sub1 myb 1 0
    else ());
    (if (logand 0x0100000000000000L myb) = 0x0100000000000000L then
      res := !res + get_fs_left_sub2 myb 1 0
    else ());
    !res

let get_fs_right myb opb = 
  let rec get_fs_right_sub1 myb n res =
    if (logand (shift_right_logical myb (7+n*8)) 0x1L) = 0x1L && n <= 6 then
      get_fs_right_sub1 myb (n+1) (res+1)
    else 
      res
  in
  let rec get_fs_right_sub2 myb n res =
    if (logand (shift_right_logical myb (63-n*8)) 0x1L) = 0x1L && n <= 6 then
      get_fs_right_sub2 myb (n+1) (res+1)
    else 
      res
  in
  if (logand 0x8080808080808080L (logor myb opb)) = 0x8080808080808080L then
    popcount (logand myb 0x0080808080808000L)
  else 
    let res = ref 0 in
    (if (logand 0x0000000000000080L myb) = 0x80L then
      res := !res + get_fs_right_sub1 myb 1 0
    else ());
    (if (logand 0x8000000000000000L myb) = 0x8000000000000000L then
      res := !res + get_fs_right_sub2 myb 1 0
    else ());
    !res

let get_fs board = 
  let (myb,opb) = board in
  if cornercount (logor myb opb) = 0 then 0
  else 
    cornercount myb + get_fs_up myb opb + get_fs_down myb opb + get_fs_left myb opb + get_fs_right myb opb
    - (cornercount opb + get_fs_up opb myb + get_fs_down opb myb + get_fs_left opb myb + get_fs_right opb myb)

let eval_middle2 board =
  let (myb,opb) = board in
  let bp = get_bp bplist board in
  let fs = get_fs board in
  let cn = popcount (valid_move board) - popcount (valid_move (opb,myb)) in
  bp * wbp + fs * wfs + cn * wcn

let sort_last ms board = 
    List.sort (fun (i1,j1) (i2,j2) -> 
      let x = popcount (valid_move (boardchange (flip board i1 j1))) in
      let y = popcount (valid_move (boardchange (flip board i2 j2))) in
      x - y) ms




