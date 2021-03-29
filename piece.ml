(** Representation of static chess pieces.

    This module represents the chess pieces used by the game state and
    board. *)

(** The abstract type of values representing chess pieces *)
type t

(** The type of owner identifier *)
type owner_id = bool

(** The type of piece identifier *)
type piece_id =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

(** [make_piece owner_id piece_id] creates a piece of type [piece_id]
    belonging to [owner_id]. *)
let make_piece color id = failwith "unimplemented"

let get_owner piece = failwith "unimplemented"

let get_piece piece = failwith "unimplemented"

let promote_piece piece id = failwith "unimplemented"
