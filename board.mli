open Piece

(** [r] represents the squares of a chess board. *)
type r

(** [t] represents a full chess position, including the squares, pieces
    on those squares, castling rights, turn, en passant rights, and all
    previous board states. *)
type t

(** [pieces] represents the pieces remaining on the board. *)
type pieces = {
  pawns : int;
  knights : int;
  bishops : int;
  rooks : int;
  queens : int;
}

exception IllegalMove of string

exception EmptyMoveStack

exception IllegalSquare of string

exception IllegalPiece of string

exception IllegalFen of string

(** [init ()] returns the representation of initial position of a chess
    game. *)
val init : unit -> t

(** [get_piece str pos] takes the [str] coordinate (rankcol) square
    representation and returns the name of the piece on that square in
    [pos]. Raises: [IllegalSquare str] if [str] is not a valid chess
    square. *)
val get_piece : string -> t -> string

(** [get_pieces pos] returns the kinds remaining pieces on [pos] and who
    owns them as (White's pieces, Black's pieces). *)
val get_pieces : t -> pieces * pieces

(** [move str promote_str pos] takes in a coordinate-based move [str]
    and a promotion string [promote_str] and returns the new state after
    the move if it is legal on board [pos].

    Raises: [IllegalMove] if the move is illegal, or [IllegalSquare str]
    if one of the squares is out of bounds.

    Requires: str is a valid coordinate-based move of the form
    '[a-g][1-8][a-g][1-8]' and [promote_str] is either the empty string
    or one of "Q", "K", "R", "B", or "N". *)
val move : string -> string -> t -> t

(** [eval_move] returns whether the string move is legal in [t]. *)
val eval_move : string -> t -> bool

(** [undo_prev pos] takes in a board state [pos] and returns the
    previous board state. Raises: [EmptyMoveStack] if the move stack is
    empty. *)
val undo_prev : t -> t

(** [get_turn t] is [true] if it is White's turn, [false] if it is
    Black's. *)
val get_turn : t -> bool

(** [get_castling pos] returns a list containing values corresponding to
    the validity of castling in board state [pos]. EX:
    [white kingside castling, white queenside castling, black kingside castling, black queenside castling] *)
val get_castling : t -> bool list

(** [to_string pos] outputs a pretty-printed string of board state
    [pos]. *)
val to_string : t -> string

(** [equals pos1 pos2] returns whether board states [pos1] and [pos2]
    are equal. *)
val equals : t -> t -> bool

(** [fen_to_board fen] takes in a fen representation of a board and
    returns the board state equivalent. Requires: [fen] is a valid FEN
    format string. *)
val fen_to_board : string -> t

(** [move_list moves board] performs every move in [moves] in board
    state [board]. *)
val move_list : (string * string) list -> t -> t

(** [is_in_check pos] returns whether the current player is in check in
    board state [pos]. *)
val is_in_check : t -> bool

(** [revert_prev board x] returns the game at turn x Requires: x is less
    than than or equal to the current turn *)
val revert_prev : t -> int -> t

(** [get_turn_num t] returns the current turn number of t*)
val get_turn_num : t -> int

(** [move_generator t] returns the possible moves of pos t*)
val move_generator : t -> string list

(** [checkmate t] returns true if the player is checkmated *)
val checkmate : t -> bool

(**[draw t] returns true if the player to move can claim a draw, else
   false*)
val draw : t -> bool

(**[insufficient_material t] returns true if the player to move can claim a draw by insufficient material, else false*)
val insufficient_material : t -> bool

(**[threefold_repetition t] returns true if the player to move can claim a draw by threefold repetition, else false*)
val threefold_repetition : t -> bool

(**[fiftyfold_rule t] returns true if the player to move can claim a draw by the fiftyfold rule*)
val fiftyfold_rule : t -> bool

(**[ get_moves t] returns the moves made in the game so far*)
val get_moves : t -> (string * string) list

(**[to_fen t] returns the FEN of a chess position*)
val to_fen : t -> string
