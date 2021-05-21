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

let bot_prompt board =
  let player_turn = turn board in
  print_endline (player_turn ^ " to move: \n");
  print_endline
    " 1. Enter a move in the format '[a-g][1-8][a-g][1-8]' to indicate \
     the square to move from and to respectively\n\
    \ 2. Enter 'help' to gather possible moves\n\
    \ 3. Enter 'moves' to review the moves made\n\
    \ 4. Enter 'random' to get a random available move\n\
    \ 5. Enter 'QUIT' to exit"

let quit_game () =
  ANSITerminal.print_string [ ANSITerminal.cyan ] "Thanks for playing\n";
  exit 0

let rec play_bot board () : unit =
  bot_prompt board;
  match String.lowercase_ascii (String.trim (read_line ())) with
  | "quit" -> quit_game ()
  | "help" ->
      print (move_generator board);
      play_bot board ()
  | "moves" ->
      return_moves 1 (get_moves board);
      play_bot board ()
  | "random" ->
      print_endline (random_move board ());
      play_bot board ()
  | str -> bot_move_loop str board

and bot_move_loop str board : unit =
  ( try
      let mod_board = play_move str board in
      ANSITerminal.erase Screen;
      Board.draw_board mod_board;
      game_end_bot mod_board;
      let bot_board = play_move (minimax mod_board 1) mod_board in
      Board.draw_board bot_board;
      game_end_bot bot_board;
      play_bot bot_board ()
    with IllegalMove k ->
      ANSITerminal.print_string [ ANSITerminal.red ] k );
  print_endline "";
  play_bot board ()

and game_end_bot board =
  if is_in_check board then check_checkmate_bot board
  else if checkmate board then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      " Draw by stalemate! \n";
    exit 0 )
  else if draw board then check_draw_bot board

and check_checkmate_bot board =
  if not (checkmate board) then
    ANSITerminal.print_string [ ANSITerminal.red ]
      (turn board ^ " is in check\n")
  else (
    ANSITerminal.print_string [ ANSITerminal.red ]
      (turn board ^ " has been checkmated! \n");
    exit 0 )

and check_draw_bot board =
  if insufficient_material board then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      " Draw by insufficient material! \n";
    exit 0 )
  else if fiftyfold_rule board then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      " Draw by threefold repetition! \n";
    exit 0 )
  else (
    ANSITerminal.print_string [ ANSITerminal.red ]
      " Draw by threefold repetition! \n";
    exit 0 )

let two_player_prompt board =
  let player_turn = turn board in
  print_endline (player_turn ^ " to move: \n");
  print_endline
    " 1. Enter a move in the format '[a-g][1-8][a-g][1-8]' to indicate \
     the square to move from and to respectively\n\
    \ 2. Enter 'undo' to undo the previous move\n\
    \ 3. Enter 'revert' to choose a turn to return to \n\
    \ 4. Enter 'help' to gather possible moves\n\
    \ 5. Enter 'moves' to review the moves made\n\
    \ 6. Enter 'random' to recieve a random available move\n\
    \ 7. Enter 'QUIT' to exit"

(** [game_loop board ()] manages the game loop. *)
let rec game_loop board b =
  if b || get_turn board then (
    two_player_prompt board;
    match String.lowercase_ascii (String.trim (read_line ())) with
    | "quit" -> quit_game ()
    | "undo" -> undo_loop board b
    | "help" ->
        print (move_generator board);
        game_loop board b
    | "revert" ->
        print_endline "Enter the turn to return to";
        revert_loop board b
    | "moves" ->
        return_moves 1 (get_moves board);
        game_loop board b
    | "random" ->
        print_endline (random_move board ());
        game_loop board b
    | str -> move_loop str board b )
  else random_loop board b

and random_loop board b =
  let next_move = random_move board () in
  let next_board = move next_move "" board in
  Board.draw_board board;
  print_endline ("\nBlack moved : " ^ next_move ^ "\n");
  game_loop next_board b

and undo_loop board b =
  let old_board = undo_prev board in
  Board.draw_board old_board;
  game_loop old_board b

and revert_loop board b =
  let temp = read_line () in
  try
    let turn = int_of_string temp in
    if turn < fullmove_clock board then (
      let old_board = revert_prev board turn in
      Board.draw_board old_board;
      game_loop old_board b )
    else
      print_endline
        (temp ^ " is greater than or equal to current turn \n");
    game_loop board b
  with exn ->
    print_endline
      (temp ^ " was not a valid int, continuing current game \n");
    game_loop board b

and move_loop str board b =
  ( try
      let mod_board = play_move str board in
      ANSITerminal.erase Screen;
      Board.draw_board mod_board;
      if is_in_check mod_board then check_checkmate mod_board b
      else if checkmate mod_board then (
        ANSITerminal.print_string [ ANSITerminal.red ]
          " Draw by stalemate! \n";
        exit 0 )
      else if draw mod_board then check_draw board
      else game_loop mod_board b
    with IllegalMove k ->
      ANSITerminal.print_string [ ANSITerminal.red ] k );
  print_endline "";
  game_loop board b

and game_end board b =
  if is_in_check board then check_checkmate board b
  else if draw board then check_draw board

and check_checkmate board b =
  if not (checkmate board) then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      (turn board ^ " is in check\n");
    game_loop board b )
  else (
    ANSITerminal.print_string [ ANSITerminal.red ]
      (turn board ^ " has been checkmated! \n");
    exit 0 )

and check_draw board =
  if insufficient_material board then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      " Draw by insufficient material! \n";
    exit 0 )
  else if fiftyfold_rule board then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      " Draw by threefold repetition! \n";
    exit 0 )
  else if stalemate board then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      " Draw by stalemate! \n";
    exit 0 )
  else (
    ANSITerminal.print_string [ ANSITerminal.red ]
      " Draw by threefold repetition! \n";
    exit 0 )

let rec random_game board x =
  if x <= 0 then (
    print_endline "Success";
    exit 0 )
  else if checkmate board || draw board then (
    if is_in_check board then (
      Board.draw_board board;
      print_endline "Checkmate!";
      random_game (Board.init ()) (x - 1) )
    else Board.draw_board board;
    print_endline "Stalemate!";
    random_game (Board.init ()) x )
  else
    let a = random_move board () in
    try
      let b = move a "Q" board in
      Board.draw_board b;
      random_game b x
    with exn ->
      print_endline a;
      exit 0

let initial_prompt () =
  print_endline
    "Enter 'start' to play against a friend from the starting \
     position. Enter 'bot' to play against the engine. Enter 'random' \
     to play against a computer making random moves. Enter a FEN \
     formatted string to load a specific board. \n"

(** [start ()] initializes the board. *)
let rec start () =
  initial_prompt ();
  match String.trim (read_line ()) with
  | "start" ->
      let board = Board.init () in
      Board.draw_board board;
      game_loop board true
  | "random" ->
      Board.draw_board (Board.init ());
      game_loop (Board.init ()) false
  | "random game" -> random_game (Board.init ()) 100
  | "bot" ->
      Board.draw_board (Board.init ());
      play_bot (Board.init ()) ()
  | str ->
      start_loop str;
      print_endline "";
      start ()

and start_loop str =
  try loop_move str with
  | IllegalFen k ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        ("This is not a proper FEN: " ^ k)
  | Invalid_argument l ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "That is not a proper command. "

and loop_move str =
  let board = load_fen str in
  Board.draw_board board;
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
    ANSITerminal.print_string [ ANSITerminal.red ] " Stalemate ! \n";
    exit 0 )
  else game_loop board true

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
  resize_window 900 900;
  draw_board (Board.init ());
  main ()
