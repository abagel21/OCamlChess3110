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

(**[move] takes in a UCI format move and a board state and outputs the modified state if it is legal, otherwise throws [IllegalMove]*)
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

(**[promote a t] promotes the pawn in the eighth rank (from the perspective of the player [turn]) to a*)
val promote : string -> t -> t
