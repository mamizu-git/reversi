open Eval
open Board
open Const
open Command
open Int64
open Color

let max a b : int = 
  if a < b then b else a

let rec sc_middle (value,move) ms board depth a b flag lim =
    if (depth = 0) || (emptycount board = 0) then
      (eval_middle2 board, Pass)
    else
      match ms with 
      | [] -> (value,move)
      | (i,j)::nms ->
        if lim < (Unix.gettimeofday () -. !start_time) then
          raise Timeout
        else 
          let (myb,opb) = flip board i j in
          let newboard = (opb,myb) in
          let newms = sort_last (valid_move_list newboard) newboard in
          if newms = [] then
            let newboard = (myb,opb) in
            let newms = sort_last (valid_move_list newboard) newboard in
            if newms = [] then
              if eval_last2 newboard = Win then
                (10000, Mv (i,j))
              else 
                sc_middle (-100000,Pass) nms board depth a b 0 lim
            else
              let a = max a value in
              let (v,_) = sc_middle (-100000,Pass) newms newboard (depth-1) a b 1 lim in
              let (value, move) = if value < v then (v, Mv (i,j)) else (value, move) in
              if value >= b then 
                (value, move)
            else
              sc_middle (value,move) nms board depth a b 0 lim
          else
            if flag = 1 then
              let a = max a value in
              let (v,_) = sc_middle (-100000,Pass) newms newboard (depth-1) (-b) (-a) 1 lim in
              let (value, move) = if value < (-v) then (-v, Mv (i,j)) else (value, move) in
              if value >= b then 
                (value, move)
              else
                sc_middle (value,move) nms board depth a b 0 lim
            else
              let a = max a value in
              let (v2,_) = sc_middle (-100000,Pass) newms newboard (depth-1) (-a-1) (-a) 1 lim in
              let vsc = ref (-v2) in
              (if a < !vsc && !vsc < b then
                let (v,_) = sc_middle (-100000,Pass) newms newboard (depth-1) (-b) (-(!vsc)) 1 lim in
                vsc := (-v)
              else ());
              let (value, move) = if value < !vsc then (!vsc, Mv (i,j)) else (value, move) in
              if value >= b then
                (value, move)
              else
                sc_middle (value,move) nms board depth a b 0 lim

let rec ab_last2 (bo,move) ms board depth =
  if (depth = 0) || (emptycount board = 0) then
    (eval_last2 board, Pass)
  else
    match ms with
    | [] -> (bo,move)
    | (i,j)::nms ->
      if (!remaining_time /. 2.) -. 1. < (Unix.gettimeofday () -. !start_time) then
        ((if !lastflag = 1 then
          (last := !last - 1; lastflag := 2)
        else if !lastflag = 2 then
          ()
        else if (!turncolor = black && !last mod 2 = 0) || (!turncolor = white && !last mod 2 = 1) then 
          (last := !last - 1; lastflag := 1)
        else
          (last := !last - 2; lastflag := 1));
        raise Timeout)
      else 
        let (myb,opb) = flip board i j in
        let newboard = (opb,myb) in
        let newms = sort_last (valid_move_list newboard) newboard in
        if newms = [] then
          let newboard = (myb,opb) in
          let newms = sort_last (valid_move_list newboard) newboard in
          if newms = [] then
            if eval_last2 newboard = Win then
              (Win, Mv (i,j))
            else 
              ab_last2 (eval_last2 newboard,Pass) nms board depth
          else 
            let (v,_) = ab_last2 (Lose,Pass) newms newboard (depth-1) in
            let (bo, move) = if v = Win then (Win, Mv (i,j)) else (v, move) in
            if bo = Win then
              (bo, move)
            else
              ab_last2 (bo,move) nms board depth
        else
          let (v,_) = ab_last2 (Lose,Pass) newms newboard (depth-1) in
          let (bo, move) = if v = Lose then (Win, Mv (i,j)) else if v = Tie then (Tie, move) else (bo, move) in
          if bo = Win then
            (bo, move)
          else
            ab_last2 (bo,move) nms board depth

let rec ab_last3 (bo,move) ms board depth =
  if (depth = 0) || (emptycount board = 0) then
    (eval_last2 board, Pass)
  else
    match ms with
    | [] -> (bo,move)
    | (i,j)::nms ->
      if (!remaining_time /. 2.) -. 1. < (Unix.gettimeofday () -. !start_time) then
        raise Timeout
      else 
        let (myb,opb) = flip board i j in
        let newboard = (opb,myb) in
        let newms = sort_last (valid_move_list newboard) newboard in
        if newms = [] then
            let newboard = (myb,opb) in
            let newms = sort_last (valid_move_list newboard) newboard in
            if newms = [] then
              if eval_last2 newboard = Win || eval_last2 newboard = Tie then
                (eval_last2 newboard, Mv (i,j))
              else 
                ab_last3 (eval_last2 newboard,Pass) nms board depth
            else 
              let (v,_) = ab_last3 (Lose,Pass) newms newboard (depth-1) in
              let (bo, move) = if v = Win || v = Tie then (v, Mv (i,j)) else (bo, move) in
              if bo = Win || bo = Tie then
                (bo, move)
              else
                ab_last3 (bo,move) nms board depth
        else
          let (v,_) = ab_last2 (Lose,Pass) newms newboard (depth-1) in
          let (bo, move) = if v = Lose then (Win, Mv (i,j)) else if v = Tie then (Tie, Mv (i,j)) else (bo, move) in
          if bo = Win || bo = Tie then
            (bo, move)
          else
            ab_last3 (bo,move) nms board depth