open Board

(** [turn board] returns the current players move on [board]. *)
let turn board =
  match Board.get_turn board with true -> "White" | false -> "Black"

(** [print_board board] pretty-prints [board]. *)
let print_board board = print_endline (Board.to_string board ^ "\n")

(** [play_move str board] parses a standard chess move in UCI long
    algebraic notation from [str] and moves the piece on [board]. *)
let play_move str board =
  match String.length (String.trim str) with
  | 4 -> Board.move (String.sub str 0 4) "" board
  | 5 -> Board.move (String.sub str 0 4) (String.sub str 4 1) board
  | _ -> raise (IllegalMove str)


let rec print = function
| h :: t -> if h <> "" then print_string h; print_string "  "; print t
| [] -> print_endline "\n"
(** [game_loop board ()] manages the game loop. *)
let rec game_loop board () =
  let player_turn = turn board in
  print_endline (player_turn ^ " to move:");
  print_endline
    "Enter a move in the format '[a-g][1-8][a-g][1-8]' to indicate the \
     square to move from and to respectively, enter 'undo' to undo the \
     previous move, enter 'revert' to choose a turn to return to, \
     enter 'help' to gather possible moves, or \n
     enter  'QUIT' to exit";
  match String.trim (read_line ()) with
  | "QUIT" ->
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        "Thanks for playing\n";
      exit 0
  | "undo" ->
      let old_board = undo_prev board in
      print_board old_board;
      game_loop old_board ()
  | "help" -> 
      print (move_generator board);
      game_loop board ()
  | "revert" -> (
      print_endline "Enter the turn to return to";
      let temp = read_line () in
      try
        let turn = int_of_string temp in 
        if (turn < get_turn_num board) then 
        let old_board = revert_prev board turn in
        print_board old_board;
        game_loop old_board ()
        else 
          print_endline (temp ^ " is greater than or equal to current turn"); 
          game_loop board ()
      with exn -> game_loop board ())
  | str ->
      (try
         let mod_board = play_move str board in
         ANSITerminal.erase Screen;
         print_board mod_board;
         if is_in_check mod_board then
          if (not (checkmate mod_board)) then 
           ANSITerminal.print_string [ ANSITerminal.red ]
             (turn mod_board ^ " is in check\n")
          else  
            ANSITerminal.print_string [ ANSITerminal.red ] 
            (turn mod_board ^ " has been checkmated! \n")
         else ();
         if (checkmate mod_board) then exit 0 else
         game_loop mod_board ()
       with IllegalMove k ->
         ANSITerminal.print_string [ ANSITerminal.red ] k);
      print_endline "";
      game_loop board ()

(** [start ()] initializes the board. *)
let rec start () =
  print_endline
    "Enter 'start' to begin with the starting chess position or enter \
     a FEN formatted string to load a specific board\n";
  match String.trim (read_line ()) with
  | "start" ->
      let board = Board.init () in
      print_board board;
      game_loop board ()
  | str ->
      begin
        try
          let board = fen_to_board str in
          print_board board;
          game_loop board ()
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
    "Welcome to OCamlChess";
  print_endline "";
  start ()

(* Execute the game engine. *)
let () = main ()
