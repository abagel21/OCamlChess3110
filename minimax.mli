open Board

(** [minimax pos depth] takes in a board state [pos] and determines
    the best move by running through every legal move up to a certain [depth]
    while maximizing the player's valuation and minimizing the opponents.  *)
val minimax : Board.t -> int -> string