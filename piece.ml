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

(** The associated FEN representations of pieces. *)
let fen_of_pieces =
  [
    (Pawn, "P");
    (Rook, "R");
    (Knight, "N");
    (Bishop, "B");
    (Queen, "Q");
    (King, "K");
  ]

let to_string p =
  let str = List.assoc p.piece_id fen_of_pieces in
  if p.color then str else String.uppercase_ascii str
