open Color

let joseki = Hashtbl.create 10000000

let bookhist = ref ""

let rec read ch hist = 
  let c = input_char ch in
  if c = '\n' then
    read ch ""
  else 
    let code = (Char.code c) - 33 in
    Hashtbl.add joseki hist code; 
    read ch (hist ^ (Char.escaped c))

let make_joseki color = 
  if color = black then
    let ch = open_in "./joseki_black.gam" in 
    try 
      read ch ""
    with
    | End_of_file -> close_in ch
  else 
    let ch = open_in "./joseki_white.gam" in 
    try 
      read ch ""
    with
    | End_of_file -> close_in ch