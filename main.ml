open Board
open Random
open Minimax
open Graphics

(** [turn board] returns the current players move on [board]. *)
let turn board =
  match Board.get_turn board with true -> "White" | false -> "Black"

(** [print_board board] pretty-prints [board]. *)
let print_board board = print_endline (Board.to_string board ^ "\n")

(** [play_move str board] parses a standard chess move in UCI long
    algebraic notation from [str] and moves the piece on [board]. *)
let play_move str board =
  match String.length (String.trim str) with
  | 4 -> Board.move (String.sub str 0 4) "Q" board
  | 5 -> Board.move (String.sub str 0 4) (String.sub str 4 1) board
  | _ -> raise (IllegalMove str)

let rec return_moves num move_list =
  match move_list with
  | [] -> print_endline "\n"
  | h :: t ->
      print_endline (string_of_int num ^ ". " ^ fst h ^ snd h);
      return_moves (num + 1) t

let rec print = function
  | h :: t ->
      if h <> "" then print_string h;
      print_string "  ";
      print t
  | [] -> print_endline "\n"

let gen_random x =
  Random.self_init ();
  Random.int x

let random_move board () =
  let d = move_generator board in
  let a = gen_random (List.length d) in
  List.nth d a

let rec play_bot board () =
  let player_turn = turn board in
  print_endline (player_turn ^ " to move: \n");
  print_endline
    " 1. Enter a move in the format '[a-g][1-8][a-g][1-8]' to indicate \
     the square to move from and to respectively\n\
    \ 2. Enter 'help' to gather possible moves\n\
    \ 3. Enter 'moves' to review the moves made\n\
    \ 4. Enter 'random' to get a random available move\n\
    \ 5. Enter 'QUIT' to exit";
  match String.lowercase_ascii (String.trim (read_line ())) with
  | "quit" ->
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        "Thanks for playing\n";
      exit 0
  | "help" ->
      print (move_generator board);
      play_bot board ()
  | "moves" ->
      return_moves 1 (get_moves board);
      play_bot board ()
  | "random" ->
      print_endline (random_move board ());
      play_bot board ()
  | str ->
      ( try
          let mod_board = play_move str board in
          ANSITerminal.erase Screen;
          print_board mod_board;
          if is_in_check mod_board then
            if not (checkmate mod_board) then
              ANSITerminal.print_string [ ANSITerminal.red ]
                (turn mod_board ^ " is in check\n")
            else (
              ANSITerminal.print_string [ ANSITerminal.red ]
                (turn mod_board ^ " has been checkmated! \n");
              exit 0 )
          else if checkmate mod_board then (
            ANSITerminal.print_string [ ANSITerminal.red ]
              " Draw by stalemate! \n";
            exit 0 )
          else if draw mod_board then
            if insufficient_material mod_board then (
              ANSITerminal.print_string [ ANSITerminal.red ]
                " Draw by insufficient material! \n";
              exit 0 )
            else if fiftyfold_rule mod_board then (
              ANSITerminal.print_string [ ANSITerminal.red ]
                " Draw by threefold repetition! \n";
              exit 0 )
            else (
              ANSITerminal.print_string [ ANSITerminal.red ]
                " Draw by threefold repetition! \n";
              exit 0 )
          else
            let bot_board = play_move (minimax mod_board 1) mod_board in
            print_board bot_board;
            if is_in_check bot_board then
              if not (checkmate bot_board) then
                ANSITerminal.print_string [ ANSITerminal.red ]
                  (turn bot_board ^ " is in check\n")
              else (
                ANSITerminal.print_string [ ANSITerminal.red ]
                  (turn bot_board ^ " has been checkmated! \n");
                exit 0 )
            else if checkmate bot_board then (
              ANSITerminal.print_string [ ANSITerminal.red ]
                " Draw by stalemate! \n";
              exit 0 )
            else if draw bot_board then
              if insufficient_material bot_board then (
                ANSITerminal.print_string [ ANSITerminal.red ]
                  " Draw by insufficient material! \n";
                exit 0 )
              else if fiftyfold_rule bot_board then (
                ANSITerminal.print_string [ ANSITerminal.red ]
                  " Draw by threefold repetition! \n";
                exit 0 )
              else (
                ANSITerminal.print_string [ ANSITerminal.red ]
                  " Draw by threefold repetition! \n";
                exit 0 )
            else play_bot bot_board ()
        with IllegalMove k ->
          ANSITerminal.print_string [ ANSITerminal.red ] k );
      print_endline "";
      play_bot board ()

(** [game_loop board ()] manages the game loop. *)
let rec game_loop board () b =
  if b || get_turn board then (
    let player_turn = turn board in
    print_endline (player_turn ^ " to move: \n");
    print_endline
      " 1. Enter a move in the format '[a-g][1-8][a-g][1-8]' to \
       indicate the square to move from and to respectively\n\
      \ 2. Enter 'undo' to undo the previous move\n\
      \ 3. Enter 'revert' to choose a turn to return to \n\
      \ 4. Enter 'help' to gather possible moves\n\
      \ 5. Enter 'moves' to review the moves made\n\
      \ 6. Enter 'random' to recieve a random available move\n\
      \ 7. Enter 'QUIT' to exit";
    match String.lowercase_ascii (String.trim (read_line ())) with
    | "quit" ->
        ANSITerminal.print_string [ ANSITerminal.cyan ]
          "Thanks for playing\n";
        exit 0
    | "undo" ->
        let old_board = undo_prev board in
        print_board old_board;
        game_loop old_board () b
    | "help" ->
        print (move_generator board);
        game_loop board () b
    | "revert" -> (
        print_endline "Enter the turn to return to";
        let temp = read_line () in
        try
          let turn = int_of_string temp in
          if turn < get_turn_num board then (
            let old_board = revert_prev board turn in
            print_board old_board;
            game_loop old_board () b )
          else
            print_endline
              (temp ^ " is greater than or equal to current turn \n");
          game_loop board () b
        with exn ->
          print_endline
            (temp ^ " was not a valid int, continuing current game \n");
          game_loop board () b )
    | "moves" ->
        return_moves 1 (get_moves board);
        game_loop board () b
    | "random" ->
        print_endline (random_move board ());
        game_loop board () b
    | str ->
        ( try
            let mod_board = play_move str board in
            ANSITerminal.erase Screen;
            print_board mod_board;
            if is_in_check mod_board then
              if not (checkmate mod_board) then
                ANSITerminal.print_string [ ANSITerminal.red ]
                  (turn mod_board ^ " is in check\n")
              else (
                ANSITerminal.print_string [ ANSITerminal.red ]
                  (turn mod_board ^ " has been checkmated! \n");
                exit 0 )
            else if checkmate mod_board then (
              ANSITerminal.print_string [ ANSITerminal.red ]
                " Draw by stalemate! \n";
              exit 0 )
            else if draw mod_board then
              if insufficient_material mod_board then (
                ANSITerminal.print_string [ ANSITerminal.red ]
                  " Draw by insufficient material! \n";
                exit 0 )
              else if fiftyfold_rule mod_board then (
                ANSITerminal.print_string [ ANSITerminal.red ]
                  " Draw by threefold repetition! \n";
                exit 0 )
              else (
                ANSITerminal.print_string [ ANSITerminal.red ]
                  " Draw by threefold repetition! \n";
                exit 0 )
            else game_loop mod_board () b
          with IllegalMove k ->
            ANSITerminal.print_string [ ANSITerminal.red ] k );
        print_endline "";
        game_loop board () b )
  else
    let c = random_move board () in
    let d = move c "" board in
    print_board d;
    print_endline ("\nBlack moved : " ^ c ^ "\n");
    game_loop d () b

let rec random_game board x =
  if x <= 0 then (
    print_endline "Success";
    exit 0 )
  else if checkmate board || draw board then (
    if is_in_check board then (
      print_board board;
      print_endline "Checkmate!";
      random_game (Board.init ()) (x - 1) )
    else print_board board;
    print_endline "Stalemate!";
    random_game (Board.init ()) x )
  else
    let a = random_move board () in
    try
      let b = move a "Q" board in
      print_board b;
      random_game b x
    with exn ->
      print_endline a;
      exit 0

(** [start ()] initializes the board. *)
let rec start () =
  print_endline
    "Enter 'start' to play against a friend from the starting \
     position. Enter 'bot' to play against the engine. Enter 'random' \
     to play against a computer making random moves. Enter a FEN \
     formatted string to load a specific board. \n";
  match String.trim (read_line ()) with
  | "start" ->
      let board = Board.init () in
      print_board board;
      game_loop board () true
  | "random" ->
      print_board (Board.init ());
      game_loop (Board.init ()) () false
  | "random game" -> random_game (Board.init ()) 100
  | "bot" ->
      print_board (Board.init ());
      play_bot (Board.init ()) ()
  | str ->
      begin
        try
          let board = fen_to_board str in
          print_board board;
          if is_in_check board then
            if not (checkmate board) then
              ANSITerminal.print_string [ ANSITerminal.red ]
                (turn board ^ " is in check\n")
            else (
              ANSITerminal.print_string [ ANSITerminal.red ]
                (turn board ^ " has been checkmated! \n");
              exit 0 )
          else ();
          if checkmate board then (
            ANSITerminal.print_string [ ANSITerminal.red ]
              " Stalemate ! \n";
            exit 0 )
          else game_loop board () true
        with
        | IllegalFen k ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              ("This is not a proper FEN: " ^ k)
        | Invalid_argument l ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "That is not a proper command. "
      end;
      print_endline "";
      start ()

(** [main ()] delivers initial instructions and triggers the board
    setup. *)
let main () =
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "Welcome to\n     OCamlChess";
  print_endline "";
  start ()

(* Execute the game engine. *)
let () =
  set_window_title "OChessl";
  open_graph "";
  resize_window 1000 1000;
  for i = 0 to 7 do
    for j = 0 to 7 do
      if i mod 2 = 0 && j mod 2 = 0 then set_color (rgb 230 207 147)
      else if j mod 2 = 0 && i mod 2 = 1 then
        set_color (rgb 207 169 103)
      else if j mod 2 = 1 && i mod 2 = 0 then
        set_color (rgb 207 169 103)
      else if j mod 2 = 1 && i mod 2 = 1 then
        set_color (rgb 230 207 147);
      draw_rect (i * 100) (j * 100) 100 100;
      fill_rect (i * 100) (j * 100) 100 100
    done
  done;
  main ()

(** Kept the functions below in case we develop more playable modes and
    the true/false isn't enough.*)

(*let rec game_loop_random board () = if get_turn board = false then (
  let c = random_move board () in let d = move (c) "" board in
  print_endline ("Black moved : " ^c); print_board d; print_endline (
  "White to move: \n"); print_endline "1. Enter a move in the format
  '[a-g][1-8][a-g][1-8]' to indicate \ the square to move from and to
  respectively\n\ \ 2. Enter 'undo' to undo the previous move\n\ \ 3.
  Enter 'revert' to choose a turn to return to, \n\ \ 4. Enter 'help' to
  gather possible moves\n\ \ 5. Enter 'moves' to review the moves
  made\n\ \ 6. Enter 'random' to recieve a random available move\n\ \ 7.
  Enter 'QUIT' to exit"; game_loop_random d ()) else match String.trim
  (read_line ()) with | "QUIT" -> ANSITerminal.print_string [
  ANSITerminal.cyan ] "Thanks for playing\n"; exit 0 | "undo" -> let
  old_board = undo_prev board in print_board old_board; game_loop_random
  old_board () | "help" -> print (move_generator board); game_loop board
  () | "revert" -> ( print_endline "Enter the turn to return to"; let
  temp = read_line () in try let turn = int_of_string temp in if turn <
  get_turn_num board then ( let old_board = revert_prev board turn in
  print_board old_board; game_loop_random old_board () b) else
  print_endline (temp ^ " is greater than or equal to current turn \n");
  game_loop_random board () with exn -> print_endline (temp ^ " was not
  a valid int, continuing current game \n"); game_loop_random board ())
  | "moves" -> return_moves 1 (get_moves board); game_loop_random board
  () | "random" -> print_endline (random_move board ());
  game_loop_random board () | str -> (try let mod_board = play_move str
  board in ANSITerminal.erase Screen; print_board mod_board; if
  is_in_check mod_board then ( if not (checkmate mod_board) then
  ANSITerminal.print_string [ ANSITerminal.red ] (turn mod_board ^ " is
  in check\n") else ANSITerminal.print_string [ ANSITerminal.red ] (turn
  mod_board ^ " has been checkmated! \n"); exit 0) else (); if checkmate
  mod_board then ( ANSITerminal.print_string [ ANSITerminal.red ] "
  Stalemate! \n"; exit 0) else game_loop_random mod_board () with
  IllegalMove k -> ANSITerminal.print_string [ ANSITerminal.red ] k);
  print_endline ""; game_loop_random board () *)
(* let rec game_loop board () = let player_turn = turn board in
   print_endline (player_turn ^ " to move: \n"); print_endline "1. Enter
   a move in the format '[a-g][1-8][a-g][1-8]' to indicate \ the square
   to move from and to respectively\n\ \ 2. Enter 'undo' to undo the
   previous move\n\ \ 3. Enter 'revert' to choose a turn to return to,
   \n\ \ 4. Enter 'help' to gather possible moves\n\ \ 5. Enter 'moves'
   to review the moves made\n\ \ 6. Enter 'random' to recieve a random
   available move\n\ \ 7. Enter 'QUIT' to exit"; match String.trim
   (read_line ()) with | "QUIT" -> ANSITerminal.print_string [
   ANSITerminal.cyan ] "Thanks for playing\n"; exit 0 | "undo" -> let
   old_board = undo_prev board in print_board old_board; game_loop
   old_board () | "help" -> print (move_generator board); game_loop
   board () | "revert" -> ( print_endline "Enter the turn to return to";
   let temp = read_line () in try let turn = int_of_string temp in if
   turn < get_turn_num board then ( let old_board = revert_prev board
   turn in print_board old_board; game_loop old_board ()) else
   print_endline (temp ^ " is greater than or equal to current turn
   \n"); game_loop board () with exn -> print_endline (temp ^ " was not
   a valid int, continuing current game \n"); game_loop board ()) |
   "moves" -> return_moves 1 (get_moves board); game_loop board () |
   "random" -> print_endline (random_move board ()); game_loop board ()
   | str -> (try let mod_board = play_move str board in
   ANSITerminal.erase Screen; print_board mod_board; if is_in_check
   mod_board then ( if not (checkmate mod_board) then
   ANSITerminal.print_string [ ANSITerminal.red ] (turn mod_board ^ " is
   in check\n") else ANSITerminal.print_string [ ANSITerminal.red ]
   (turn mod_board ^ " has been checkmated! \n"); exit 0) else (); if
   checkmate mod_board then ( ANSITerminal.print_string [
   ANSITerminal.red ] " Stalemate! \n"; exit 0) else game_loop mod_board
   () with IllegalMove k -> ANSITerminal.print_string [ ANSITerminal.red
   ] k); print_endline ""; game_loop board ()*)
