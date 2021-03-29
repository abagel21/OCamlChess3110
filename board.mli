open Piece

(**[r] represents just the squares of a chess board*)
type r 

(**[t] represents a full chess position, including the squares, pieces on those
 squares, castling rights, turn, en passant rights, and all previous board states*)
type t


exception IllegalMove of string
exception EmptyMoveStack
exception IllegalSquare of string

(**[init] returns the representation of the initial position of a chess game*)
val init: t

(**[get_piece str t] takes the [str] coordinate (rankcol) string square 
representation and returns the piece on that square in [t] or throws 
[IllegalSquare] if the square is not a valid chess square*)
val get_piece : string -> t -> Piece.t

(**[move str pos] takes in a coordinate-based move [str] and returns the
   new state after the move if it is legal, throws IllegalMove if the
   move is illegal, throws IllegalSquare if one of the squares is out of*)
val move : string -> t -> t

(**[eval_move] returns true if the string move is legal in [t], otherwise false*)
val eval_move : string -> t -> bool

(**[undo_prev] takes in a board state and returns the previous board state, 
  or throws [EmptyMoveStack] if the move stack is empty*)
val undo_prev : t -> t

(**[turn t] outputs a boolean variable representing the turn*)
val get_turn : t -> bool

(**[to_string t] outputs the pretty-printed string of the board state*)
val to_string : t -> string

(**[promote a t] promotes the pawn in the eighth rank (from the perspective of
 the player [turn]) to a*)
val promote : string -> t -> t

(**[equals t t] checks if two boards are equal and returns true if they are, 
else false*)
val equals : t -> t -> bool

(**[fen_to_board fen board] takes in a fen representation of a board and 
returns the board state equivalent.
Precondition: [fen] is a valid FEN format string
*)
val fen_to_board : string -> t
