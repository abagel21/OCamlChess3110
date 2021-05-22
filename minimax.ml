(*open Board

  (** [point_aux pos] calculates the total point values of pieces on
  board [pos]. *) let point_aux pos = let w_pieces, b_pieces =
  Board.get_pieces pos in let calc_aux pieces = (9 * pieces.queens) + (5
  * pieces.rooks) + (3 * (pieces.bishops + pieces.knights)) in (calc_aux
  w_pieces, calc_aux b_pieces)

  (** [point_calc m pos] calculates the piece value difference between
  White and Black if move [m] is taken on board [pos]. *) let point_calc
  m pos = let next_board = Board.move m "Q" pos in let points =
  point_aux next_board in Board.undo_prev next_board; (fst points + snd
  points)

  (** [heuristic pos] returns a pair with the move that maximizes the
  point values obtained by the current player on a board [pos], and the
  value is what the move is worth. *) let heuristic pos = let w_turn =
  Board.get_turn pos in if Board.checkmate pos then (* checkmate gives
  highest int if white, lowest if black *) ("", if w_turn then
  Int.max_int else Int.min_int) else let moves = Board.move_generator
  pos in moves |> List.map (fun m -> (m, point_calc m pos)) |> List.sort
  (fun (_, p) (_, p') -> if w_turn then compare p p' else compare p' p)
  |> List.hd

  (* list never empty *)

  (** [max_turn pos depth] determines the best value for the maximizing
  player on a board [pos] until the search has reached [depth]. *) let
  rec max_turn pos depth = if depth = 0 || Board.checkmate pos then
  heuristic pos else let actions = Board.move_generator pos in let
  max_val = ref Int.min_int in let best = ref "" in for a = 0 to
  List.length actions - 1 do let next_board = Board.move (List.nth
  actions a) "" pos in let next_a, next_val = min_turn next_board (depth
  - 1) in let a', val' = (ref next_a, ref next_val) in if !val' >
  !max_val then (max_val := !val'; undo_prev next_board; best := !a')
  else (undo_prev next_board; best := !best ) done; (!best, !max_val)

  (** [min_turn pos depth] determines the best value for the minimizing
  player on a board [pos] until the search has reached [depth]. *) and
  min_turn pos depth = if depth = 0 || Board.checkmate pos then
  heuristic pos else let actions = Board.move_generator pos in let
  min_val = ref Int.max_int in let best = ref "" in for a = 0 to
  List.length actions - 1 do let next_board = Board.move (List.nth
  actions a) "" pos in let next_a, next_val = max_turn next_board (depth
  - 1) in let a', val' = (ref next_a, ref next_val) in if !val' <
  !min_val then (min_val := !val'; undo_prev next_board; best := !a')
  else (undo_prev next_board; best := !best ) done; (!best, !min_val)

  let minimax pos depth = fst ((if get_turn pos then max_turn else
  min_turn) pos depth) *)

open Board

(** [point_aux pos] calculates the total point values of pieces on board
    [pos]. *)
let point_aux pos =
  let w_pieces, b_pieces = Board.get_pieces pos in
  let calc_aux pieces =
    (9 * pieces.queens) + (5 * pieces.rooks)
    + (3 * (pieces.bishops + pieces.knights))
    + pieces.pawns
  in
  calc_aux w_pieces + calc_aux b_pieces
  + if is_in_check pos then -5 else 0
(* arbitrary to be determined *)

(** [point_calc m pos] calculates the piece value difference between
    White and Black if move [m] is taken on board [pos]. *)
let point_calc m pos =
  let orig = point_aux pos in
  let next_board = Board.move m "Q" pos in
  let points = point_aux next_board in
  Board.undo_prev next_board;
  orig - points

(** [pesto_aux m pos] calculates the score of a board [pos'] if move [m]
    is taken on board [pos]. *)
let pesto_aux m pos =
  let next_board = Board.move m "Q" pos in
  let score = Pestocaml.eval next_board in
  Board.undo_prev next_board;
  score

(** [eval m pos] is any evaluation function that returns the value of a
    board [pos'] if move [m] is taken on a board [pos]. *)
let eval = pesto_aux

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
    List.hd (* list never empty *)
      (List.sort
         (fun (_, b) (_, d) -> compare d b)
         (List.map (fun a -> (a, eval a pos)) moves))

(** [max_turn pos depth] determines the best value for the maximizing
    player on a board [pos] until the search has reached [depth]. *)
let rec max_turn pos depth =
  if depth = 0 || Board.checkmate pos then heuristic pos
  else
    let max_val = ref 0 in
    let actions = Board.move_generator pos in
    let best = ref (List.hd actions) in
    for a = 0 to List.length actions - 1 do
      let temp_move = List.nth actions a in
      let temp_points = eval temp_move pos in
      let next_board = Board.move temp_move "Q" pos in
      if checkmate next_board then (
        max_val := max_int;
        undo_prev next_board;
        best := temp_move)
      else
        let next_a, next_val = min_turn next_board (depth - 1) in
        if temp_points + next_val > !max_val then (
          best := temp_move;
          undo_prev next_board;
          max_val := temp_points + next_val)
        else (
          undo_prev next_board;
          best := !best)
      (* let next_board = Board.move (List.nth actions a) "" pos in let
         next_a, next_val = min_turn next_board (depth - 1) in let a',
         val' = (ref next_a, ref next_val) in if !val' > !max_val then
         (max_val := !val'; undo_prev next_board; best := !a') else
         (undo_prev next_board; best := !best ) *)
    done;
    (!best, !max_val)

(** [min_turn pos depth] determines the best value for the minimizing
    player on a board [pos] until the search has reached [depth]. *)
and min_turn pos depth =
  if depth = 0 || Board.checkmate pos then
    match heuristic pos with a, b -> (a, -b)
  else
    let min_val = ref 0 in
    let actions = Board.move_generator pos in
    let best = ref (List.hd actions) in
    for a = 0 to List.length actions - 1 do
      let temp_move = List.nth actions a in
      let temp_points = -eval temp_move pos in
      let next_board = Board.move temp_move "Q" pos in
      if checkmate next_board then (
        min_val := min_int;
        undo_prev next_board;
        best := temp_move)
      else
        let next_a, next_val = max_turn next_board (depth - 1) in
        if temp_points + next_val < !min_val then (
          best := temp_move;
          undo_prev next_board;
          min_val := temp_points + next_val)
        else (
          undo_prev next_board;
          best := !best)
    done;
    (!best, !min_val)

let minimax pos depth =
  fst ((if get_turn pos then max_turn else min_turn) pos depth)
