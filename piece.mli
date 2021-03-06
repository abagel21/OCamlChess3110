(** Representation of static chess pieces.

    This module represents the chess pieces used by the game state and
    board. *)

type t

(** The type of the color of a piece *)
type color_id = bool

(** The type of piece identifier *)
type piece_id =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

(** [make_piece c piece_id] creates a piece of type [piece_id] belonging
    to player [c]. *)
val make_piece : color_id -> piece_id -> t

(** [get_color t] returns the color of [t]. *)
val get_color : t -> color_id

(** [get_piece t] returns the type of piece of [t]. *)
val get_piece : t -> piece_id

(** [to_string t] returns the FEN representation of piece [t]. *)
val to_string : t -> string
