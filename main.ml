open Board

(** [turn board] returns the current players move on [board] *)
let turn board =
  match Board.get_turn board with
  | true -> "White"
  | false -> "Black"

(** [play_move str board ] parses a standard chess move in 
UCI long algebraic notation from [str] and moves the piece on [board] *)
let play_move str board = 
  match String.length str with
  | 4 -> Board.move (String.sub str 0 4) "" board
  | 5 -> Board.move (String.sub str 0 4) (String.sub str 4 1) board
  | _ -> raise (IllegalMove str)

(** [game_loop board ()] manages the game loop. *)
let rec game_loop board () =
  let player_turn = turn board in
  print_endline (player_turn ^ " to move:");
  match read_line() with
  | "QUIT" -> ()
  | str ->
    let mod_board = play_move str board in
    ANSITerminal.erase Screen;
    print_endline (Board.to_string board ^ "\n");
    game_loop mod_board ()

(** [main ()] initializes the board and prompts player for 
  their first move. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "Initializing chess board...\n";
  let board = Board.init() in
  game_loop board ()
  
(* Execute the game engine. *)
let () = main ()
