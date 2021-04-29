open Board

(* [move_generator pos] gets the possible moves on a board *)
(* we need to first generate all the moves, then for each one we should
   pass in a board that made that move *)
let heuristic pos = 5

(** [max_turn pos depth curr_val] determines the best value for the
    maximizing player on a board [pos] until the search has reached
    [depth]. *)
let rec max_turn pos depth curr_val =
  if depth = 0 then heuristic pos
  else
    let actions = Board.move_generator pos in
    actions
    (* for every action, get the maximum value from the min player's
       moves *)
    |> List.fold_left
         (fun acc m' ->
           (min_turn (Board.move m' "" pos) (depth - 1)) acc)
         curr_val
    |> max curr_val

(** [min_turn pos depth curr_val] determines the best value for the
    minimizing player on a board [pos] until the search has reached
    [depth]. *)
and min_turn pos depth curr_val =
  if depth = 0 then heuristic pos
  else
    let actions = Board.move_generator pos in
    actions
    |> List.fold_left
         (fun acc m' ->
           (max_turn (Board.move m' "" pos) (depth - 1)) acc)
         curr_val
    |> min curr_val

let minimax pos depth =
  if Board.get_turn pos then max_turn pos depth Int.min_int
  else min_turn pos depth Int.max_int
