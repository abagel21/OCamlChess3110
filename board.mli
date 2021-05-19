(**Representation of a chess game position.
    This module represents the sum of information of a chess position and the functionality required to manipulate it.
*)

open Piece

(** [r] represents the squares of a chess board and the pieces on them. *)
type r

(** [t] represents a full chess position, including the squares, pieces
    on those squares, castling rights, turn, en passant rights, halfmove clock, 
    and move sequence made to reach the position. *)
type t

(** [pieces] represents the pieces remaining on the board. *)
type pieces = {
  pawns : int;
  knights : int;
  bishops : int;
  rooks : int;
  queens : int;
}

(** [IllegalMove s] is the exception thrown when the user attempts to make an 
    illegal move*)
exception IllegalMove of string

(** [EmptyMoveStack] is the exception thrown when a user attempts to undo when 
    no previous move has been made*)
exception EmptyMoveStack

(** [IllegalSquare s] is the exception thrown when a user attempts to move from 
    a square that does not exist*)
exception IllegalSquare of string

(** [IllegalPiece s] is the exception thrown when an illegal string is fed into 
    the promote string for [move]*)
exception IllegalPiece of string

(** [IllegalFen s] is the exception thrown when an illegal fen is given to 
    [load_fen]*)
exception IllegalFen of string

(** [init ()] returns the representation of the initial position of a chess
    game. *)
val init : unit -> t

(** [get_piece str pos] takes the [str] coordinate (rankcol) square
    representation and returns the name of the piece on that square in
    [pos]. Raises: [IllegalSquare str] if [str] is not a valid chess
    square. *)
val get_piece : string -> t -> string

(** [get_pieces pos] returns the remaining pieces in [pos] and separated by 
    ownership in the form (White's pieces, Black's pieces) where each set of 
    pieces is in the form of type [pieces]. *)
val get_pieces : t -> pieces * pieces

(** [move str promote_str pos] takes in a coordinate-based move [str]
    and a promotion string [promote_str] representing a piece and returns the 
    new state after the move if it is legal on board [pos].

    Raises: [IllegalMove] if the move is illegal, or [IllegalSquare str]
    if one of the squares is out of bounds.

    Requires: str is a valid coordinate-based move of the form
    '[a-g][1-8][a-g][1-8]' and [promote_str] is either the empty string
    or one of "Q", "R", "B", or "N". *)
val move : string -> string -> t -> t

(** [undo_prev pos] takes in a board state [pos] and returns the
    previous board state. Raises: [EmptyMoveStack] if the move stack is
    empty. *)
val undo_prev : t -> t

(** [get_turn t] is [true] if it is White's turn, and [false] if it is
    Black's. *)
val get_turn : t -> bool

(** [get_castling pos] returns a list containing values corresponding to
    the validity of castling in board state [pos] in the form
    [white kingside castling, white queenside castling, 
    black kingside castling, black queenside castling] *)
val get_castling : t -> bool list

(** [to_string pos] outputs a pretty-printed string of the board state in
    [pos]. *)
val to_string : t -> string

(** [equals pos1 pos2] returns whether board states [pos1] and [pos2]
    are equal. *)
val equals : t -> t -> bool

(** [load_fen fen] takes in a fen representation of a board and
    returns the board state equivalent. Requires: [fen] is a valid FEN
    format string. *)
val load_fen : string -> t

(** [move_list moves board] performs every move in [moves] in board
    state [board]. Raises: IllegalMove if any move in [moves] is illegal, 
    IllegalPiece if any move in [moves] contains an invalid promotion string, 
    and IllegalSquare if any move in [moves] contains an illegal from/to square*)
val move_list : (string * string) list -> t -> t

(** [is_in_check pos] returns whether the current player is in check in
    board state [pos]. *)
val is_in_check : t -> bool

(** [revert_prev board x] moves the game back [x] moves. Requires: x is less 
    than the fullmove clock of [board] *)
val revert_prev : t -> int -> t

(** [get_turn_num t] returns the current turn number of t*)
val fullmove_clock : t -> int

(** [move_generator t] returns the possible moves of pos t*)
val move_generator : t -> string list

(** [checkmate t] returns true if the player is checkmated *)
val checkmate : t -> bool

(**[stalemate t] returns true if the player is stalemated*)
val stalemate : t -> bool

(**[draw t] returns true if the player to move can claim a draw, else
   false*)
val draw : t -> bool

(**[insufficient_material t] returns true if the player to move can claim a draw by insufficient material, else false*)
val insufficient_material : t -> bool

(**[threefold_repetition t] returns true if the player to move can claim a draw by threefold repetition, else false*)
val threefold_repetition : t -> bool

(**[fiftyfold_rule t] returns true if the player to move can claim a draw by the fiftyfold rule*)
val fiftyfold_rule : t -> bool

(**[ get_moves t] returns the moves made in the game so far in the form ('[a-g][1-8][a-g][1-8]', promote_str)*)
val get_moves : t -> (string * string) list

(**[to_fen t] returns the FEN representation of [t]*)
val to_fen : t -> string

(**[draw_board t] draws the current chess piece position on the open graphics context*)
val draw_board : t -> unit
