module type Board = sig

(**[r] represents just the squares of a chess board*)
type r 

(**[t] represents a full chess position, including the squares, pieces on those
 squares, castling rights, turn, en passant rights, and the previous board state*)
type t


exception IllegalMove
exception EmptyMoveStack

(**[init] returns the representation of the initial position of a chess game*)
val init: t

(**[move] takes in a UCI format move and a board state and outputs the modified state if it is legal, otherwise throws [IllegalMove]*)
val move : string -> t -> t

(**[eval_move] returns true if the string move is legal in [t], otherwise false*)
val eval_move : string -> t -> bool

(**[undo_prev] takes in a board state and returns the previous board state, 
  or throws [EmptyMoveStack] if the move stack is empty*)
val undo_prev : t -> t

(**[turn t] outputs a boolean variable representing the turn*)
val turn : t -> bool

(**[to_string t] outputs the pretty-printed string of the board state*)
val to_string : t -> string
end 