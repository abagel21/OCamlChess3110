open Board

(** [point_aux pos] calculates the total point values of pieces on board
    [pos]. *)
let point_aux pos =
  let w_pieces, b_pieces = Board.get_pieces pos in
  let calc_aux pieces =
    (9 * pieces.queens) + (5 * pieces.rooks)
    + (3 * (pieces.bishops + pieces.knights))
  in
  (calc_aux w_pieces, calc_aux b_pieces)

(** [point_calc m pos] calculates the piece value difference between
    White and Black if move [m] is taken on board [pos]. *)
let point_calc m pos =
  let points = point_aux (Board.move m "" pos) in
  fst points - snd points

(** [heuristic pos] returns a pair with the move that maximizes the
    point values obtained by the current player on a board [pos], and
    the value is what the move is worth. *)
let heuristic pos =
  let w_turn = Board.get_turn pos in
  if Board.checkmate pos then
    (* checkmate gives highest int if white, lowest if black *)
    ("", if w_turn then Int.max_int else Int.min_int)
  else
    let moves = Board.move_generator pos in
    moves
    |> List.map (fun m -> (m, point_calc m pos))
    |> List.sort (fun (_, p) (_, p') ->
           if w_turn then compare p p' else compare p' p)
    |> List.hd
(* list never empty *)

(** [max_turn pos depth] determines the best value for the maximizing
    player on a board [pos] until the search has reached [depth]. *)
let rec max_turn pos depth =
  if depth = 0 || Board.checkmate pos then heuristic pos
  else
    let actions = Board.move_generator pos in
    let max_val = ref Int.min_int in
    let best = ref "" in
    for a = 0 to List.length actions - 1 do
      let next_board = Board.move (List.nth actions a) "" pos in
      let a' = ref (fst (min_turn next_board (depth - 1))) in
      let val' = ref (snd (min_turn next_board (depth - 1))) in
      if !val' > !max_val then max_val := !val';
      best := !a'
    done;
    (!best, !max_val)

(** [min_turn pos depth] determines the best value for the minimizing
    player on a board [pos] until the search has reached [depth]. *)
and min_turn pos depth =
  if depth = 0 || Board.checkmate pos then heuristic pos
  else
    let actions = Board.move_generator pos in
    let min_val = ref Int.max_int in
    let best = ref "" in
    for a = 0 to List.length actions - 1 do
      let next_board = Board.move (List.nth actions a) "" pos in
      let a' = ref (fst (max_turn next_board (depth - 1))) in
      let val' = ref (snd (max_turn next_board (depth - 1))) in
      if !val' < !min_val then min_val := !val';
      best := !a'
    done;
    (!best, !min_val)

let minimax pos depth =
  fst ((if Board.get_turn pos then max_turn else min_turn) pos depth)
