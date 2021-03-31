(** Representation of static chess pieces.

    This module represents the chess pieces used by the game state and
    board. *)

(** The type of player color. *)
type color_id = bool

(** The type of piece identifier. *)
type piece_id =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

(** The abstract type of values representing chess pieces. *)
type t = {
  color : color_id;
  piece_id : piece_id;
}

let make_piece c id = { color = c; piece_id = id }

let get_color piece = piece.color

let get_piece piece = piece.piece_id

let promote piece id = { piece with piece_id = id }
