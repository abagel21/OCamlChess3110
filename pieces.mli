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
  | Queen
  | King

(** [make_piece owner_id piece_id] creates a piece of type [piece_id]
  belonging to [owner_id]. *)
val make_piece : owner_id -> piece_id -> t

(** [get_owner t] returns the owner_id of [t]. *)
val get_owner : t -> owner_id

(** [get_piece t] returns the piece_id of [t]. *)
val get_piece : t -> piece_id

(** [promote_piece t piece_id] promotes piece [t] to [piece_id] *)
val promote_piece : t -> piece_id -> t