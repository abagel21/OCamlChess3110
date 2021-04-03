open Board

(** [turn board] returns the current players move on [board] *)
let turn board =
  match Board.get_turn board with true -> "White" | false -> "Black"

(** [play_move str board] parses a standard chess move in UCI long
    algebraic notation from [str] and moves the piece on [board] *)
let play_move str board =
  match String.length str with
  | 4 -> Board.move (String.sub str 0 4) "" board
  | 5 -> Board.move (String.sub str 0 4) (String.sub str 4 1) board
  | _ -> raise (IllegalMove str)

(** [game_loop board ()] manages the game loop. *)
let rec game_loop board () =
  let player_turn = turn board in
  print_endline (player_turn ^ " to move:");
  print_endline
    "Enter a move in the format '[a-g][1-8][a-g][1-8]' to indicate the \
     square to move from and to respectively, enter 'undo' to undo the \
     previous move, or enter 'QUIT' to exit";
  match read_line () with
  | "QUIT" ->
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        "Thanks for playing\n";
      exit 0
  | "undo" -> game_loop (undo_prev board) ()
  | str ->
      (try
         let mod_board = play_move str board in
         ANSITerminal.erase Screen;
         print_endline (Board.to_string board ^ "\n");
         game_loop mod_board ()
       with IllegalMove k ->
         ANSITerminal.print_string [ ANSITerminal.red ] k);
      print_endline "";
      game_loop board ()

(**[start] initializes the board*)
let rec start () =
  print_endline
    "Enter 'start' to begin with the starting chess position or enter \
     a FEN formatted string to load a specific board\n";
  match read_line () with
  | "start" ->
      let board = Board.init () in
      print_endline(Board.to_string board);
      game_loop board ()
  | str ->
      (try
         let board = fen_to_board str in
         game_loop board ()
       with IllegalFen k ->
         ANSITerminal.print_string [ ANSITerminal.red ]
           ("This is not a proper FEN: " ^ k));
      print_endline "";
      start ()

(** [main ()] delivers initial instructions and triggers the board setup*)
let main () =
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "Welcome to OCamlChess";
  print_endline "";
  start ()

(* Execute the game engine. *)
let () = main ()
