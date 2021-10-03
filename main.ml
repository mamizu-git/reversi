open Unix

open Command
open Play
open Color
open Printf

open Const
open Board
open New
open Book
open Eval
open Search

let opt_verbose     = ref false
let opt_player_name = ref "Anon."
let opt_port        = ref 3000
let opt_host        = ref "localhost"

let options =
  [("-H", Arg.Set_string opt_host, "host name (default = local host)");
   ("-p", Arg.Set_int    opt_port, "port number (default = 3000)");
   ("-n", Arg.Set_string opt_player_name, "player name (default = Anon.)");
   ("-v", Arg.Set        opt_verbose, "verbose mode")]

let usage_msg =
  "Usage:\n\treversi -H HOST -p PORT -n PLAYERNAME...\n"

let input_color ic =
  let s = input_line ic in
    if s = "white" then white
    else                black

let output_color oc color =
  if color = white then (output_string oc "white\n"; flush oc)
  else                  (output_string oc "black\n"; flush oc)

let input_command ic =
  let s = input_line ic in
  let report_recv s = print_endline ("Received: " ^ s) in
  (* let _ = report_recv s in *)
  let lexbuf = Lexing.from_string s in
  CommandParser.comm CommandLexer.token lexbuf

let rec input_command' ic =
  match input_command ic with
      Empty -> input_command' ic
    | r     -> r

let output_command oc command =
  let report_sent s = print_endline ("Sent: "^s) in
  let o s = (output_string oc (s ^ "\n"); flush oc) in
  match command with
    | Move mv ->
      o ("MOVE " ^ string_of_move mv)
    | Open s ->
      o ("OPEN " ^ s)
    | _ ->
      failwith "Client cannot not send the commands other than MOVE and Open"


type opmove = PMove of move | OMove of move

let string_of_opmove = function
  | PMove mv -> "+" ^ string_of_move mv
  | OMove mv -> "-" ^ string_of_move mv

type hist = opmove list

let string_of_hist x = List.fold_left (fun r a -> string_of_opmove a ^ " " ^ r) "" x
let print_hist x = print_endline (string_of_hist x)

let string_of_scores scores =
  let maxlen =
    List.fold_right (fun (a,_) r -> max (String.length a) r) scores 0 in
  let maxslen =
    List.fold_right (fun (_,(s,_,_)) r -> max (String.length (string_of_int s)) r) scores 0 in
  List.fold_right (fun (a,(s,w,l)) r ->
    a ^ ":" ^ String.make (maxlen + 1 - String.length a) ' ' ^
       String.make (maxslen - String.length (string_of_int s)) ' ' ^ string_of_int s ^
         " (Win " ^ string_of_int w ^ ", Lose " ^ string_of_int l ^ ")\n" ^ r)
    scores ""

let print_scores scores = print_string (string_of_scores scores)



let rec wait_start (ic,oc) =
  match input_command ic with
      Bye scores ->
        print_scores scores
    | Start (color, oname, mytime) ->
      my_time := mytime;
      bookhist := "";
      waste_time := 0.;
      lastflag := 0;
      Hashtbl.clear joseki;
      let _ = make_joseki color in
      let board = init_bitboard () in
      if color = black then
        my_move (ic,oc) board color [] oname mytime
      else
        op_move (ic,oc) board color [] oname mytime
    | _ ->
      failwith "Invalid Command"

and my_move (ic,oc) board color hist oname mytime =
  remaining_time := (timelimit -. (float_of_int (!my_time - mytime)) /. 1000.);
  start_time := Unix.gettimeofday ();
  let r_pmove = play board in
  let board = doMove board r_pmove in
  let pmove = rotate_move r_pmove in
  let _ = output_command oc (Move pmove) in
  let _ = if !opt_verbose then
      (print_endline "--------------------------------------------------------------------------------";
       print_endline ("PMove: " ^ string_of_move r_pmove ^ " " ^ string_of_color color);
       print_bitboard board) in
  match input_command ic with
    | Ack mytime' ->
      op_move (ic,oc) board color (PMove r_pmove :: hist) oname mytime'
    | End (wl,n,m,r) ->
      proc_end (ic,oc) board color hist oname wl n m r
    | _ ->
      failwith "Invaid Command"

and op_move (ic,oc) board color hist oname mytime =
  match input_command ic with
    | Move omove ->
      (if emptycount board = 60 then
        match omove with
        | Mv (4,3) -> rotate := 0
        | Mv (3,4) -> rotate := 1
        | Mv (5,6) -> rotate := 2
        | Mv (6,5) -> rotate := 3
        | _ -> ()
      else ());
      let r_omove = rotate_move omove in
      let board = doMove board r_omove in
      let _ = if !opt_verbose then
          (print_endline "--------------------------------------------------------------------------------";
          print_endline ("OMove: " ^ string_of_move r_omove ^ " " ^ string_of_color (opposite_color color));
          (* print_board board *)
          print_bitboard board) in
      my_move (ic,oc) board color (OMove r_omove :: hist) oname mytime
    | End (wl,n,m,r) ->
      proc_end (ic,oc) board color hist oname wl n m r
    | _ ->
      failwith "Invalid Command"

and proc_end (ic,oc) board color hist oname wl n m r =
  let _ = match wl with
    | Win  -> printf "You win! (%d vs. %d) -- %s.\n" n m r
    | Lose -> printf "You lose! (%d vs. %d) -- %s.\n" n m r
    | Tie  -> printf "Draw (%d vs. %d) -- %s.\n" n m r in
  let _ =
    printf "Your name: %s (%s)  Oppnent name: %s (%s).\n"
      (!opt_player_name ) (string_of_color color)
      oname (string_of_color (opposite_color color)) in
  (* let _ = print_board board in *)
  let _ = print_bitboard board in
  let _ = print_hist  hist  in
  (* print_string "waste: "; print_float !waste_time; print_endline ""; *)
  (if !lastflag >= 1 && wl = Lose then
    middle_limit := !middle_limit -. 0.25
  else if wl = Lose then
    middle_limit := !middle_limit +. 0.25
  else if !waste_time > 10. then
    middle_limit := !middle_limit -. 0.1
  else 
    ());
  wait_start (ic,oc)


let client host port =
  let addr    = (gethostbyname host).h_addr_list.(0) in
  let _ = print_endline ("Connecting to " ^ host ^ " " ^ string_of_int port ^ ".") in
  let (ic,oc) = open_connection (ADDR_INET (addr, port)) in
  let _ = print_endline "Connection Ok." in
  let _ = output_command oc (Open !opt_player_name) in
  wait_start (ic,oc)

let main () =
  let _ = Random.self_init () in
  let _ = Arg.parse options (fun _ -> ()) usage_msg in
  let (host,port) = (!opt_host, !opt_port) in
    client host port

;;
main ();;
