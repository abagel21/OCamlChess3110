open Piece

(**TODO 1. implement piece to_string 2. Implement pawn movement
   (including en passant) Alex 4. implement pins (if a move causes the
   player to be in check) 6. Finish FEN function Alex 7. Testing board
   8. Command Line 9. Move under check 5. Implement checkmate/draw
   checking*)

type r = Piece.t option array array

exception IllegalSquare of string

exception IllegalMove of string

exception IllegalFen of string

exception IllegalPiece of string

exception EmptyMoveStack

type square = int * int

type move = string * string

type castling = {
  wk : bool;
  wq : bool;
  bk : bool;
  bq : bool;
}

type pieces = {
  pawns : int;
  knights : int;
  bishops : int;
  rooks : int;
  queens : int;
}

type full_move = {
  from_sqr : int * int;
  to_sqr : int * int;
  capture : Piece.t option;
  ep : bool;
  ep_sqr : int * int;
  castle : bool;
  promotion : Piece.t option;
  was_checked : bool;
  halfmove_clock : int;
  prev_castling : castling;
}

(*AF: the record {board, castling, ep, turn, move_stack, checked, wking,
  bking} represents a full chess position where
  board=[|[|a1...a8|];...;[|h1...h8|]|] represents the squares of the
  board, castling=[qw;kw;qb;kb] such that qw is a boolean representing
  whether white has queenside castling and the rest follow, ep=(int,
  int) where the first int is the row and the second int is the column
  of the en passant square, turn=bool where true is white's turn and
  false is black's turn the string list of type r represents the move
  stack of the position, move_stack is a list of moves made so far,
  checked is a boolean where true means the player is in check, wking
  and bking represent the square each king occupies*)
(*RI=The t.board array is always full*)
type t = {
  board : r;
  castling : castling;
  ep : int * int;
  turn : bool;
  checked : bool;
  wking : square;
  bking : square;
  wpieces : pieces;
  bpieces : pieces;
  move_stack : full_move list;
  fullmove_clock : int;
  halfmove_clock : int;
}

let wrook = Some (make_piece true Rook)

let wknight = Some (make_piece true Knight)

let wbishop = Some (make_piece true Bishop)

let whking = Some (make_piece true King)

let wqueen = Some (make_piece true Queen)

let wpawn = Some (make_piece true Pawn)

let brook = Some (make_piece false Rook)

let bknight = Some (make_piece false Knight)

let bbishop = Some (make_piece false Bishop)

let blking = Some (make_piece false King)

let bqueen = Some (make_piece false Queen)

let bpawn = Some (make_piece false Pawn)

let init_board_list =
  [
    [ wrook; wpawn; None; None; None; None; bpawn; brook ];
    [ wknight; wpawn; None; None; None; None; bpawn; bknight ];
    [ wbishop; wpawn; None; None; None; None; bpawn; bbishop ];
    [ wqueen; wpawn; None; None; None; None; bpawn; bqueen ];
    [ whking; wpawn; None; None; None; None; bpawn; blking ];
    [ wbishop; wpawn; None; None; None; None; bpawn; bbishop ];
    [ wknight; wpawn; None; None; None; None; bpawn; bknight ];
    [ wrook; wpawn; None; None; None; None; bpawn; brook ];
  ]

(**[init_empty_board_array] returns board array that is completely empty *)
let init_empty_board_array () = Array.make_matrix 8 8 None

(**[init_empty] returns a board representation that is empty *)
let init_empty () =
  {
    board = init_empty_board_array ();
    castling = { wk = false; wq = false; bk = false; bq = false };
    ep = (-1, -1);
    turn = true;
    checked = false;
    wking = (-1, -1);
    bking = (-1, -1);
    wpieces =
      { pawns = 0; knights = 0; bishops = 0; rooks = 0; queens = 0 };
    bpieces =
      { pawns = 0; knights = 0; bishops = 0; rooks = 0; queens = 0 };
    move_stack = [];
    halfmove_clock = 0;
    fullmove_clock = 0;
  }

let init () =
  {
    board = Array.of_list (List.map Array.of_list init_board_list);
    castling = { wk = true; wq = true; bk = true; bq = true };
    ep = (-1, -1);
    turn = true;
    checked = false;
    wking = (4, 0);
    bking = (4, 7);
    wpieces =
      { pawns = 8; knights = 2; bishops = 2; rooks = 2; queens = 1 };
    bpieces =
      { pawns = 8; knights = 2; bishops = 2; rooks = 2; queens = 1 };
    move_stack = [];
    halfmove_clock = 0;
    fullmove_clock = 0;
  }

(**[get_turn pos] returns true if it is white's move and false
   otherwise.*)
let get_turn pos = pos.turn

(**[is_in_check pos] returns true if the current player is in check*)
let is_in_check pos = pos.checked

let get_piece_helper str pos =
  let trm_str = String.trim str in
  if String.length trm_str = 2 then
    let rank = Char.code str.[0] - Char.code 'a' in
    let col = Char.code str.[1] - Char.code '1' in
    if rank > 7 || rank < 0 || col > 7 || rank < 0 then
      raise (IllegalSquare (str ^ " is not a valid square"))
    else pos.board.(rank).(col)
  else raise (IllegalSquare (str ^ " is not a valid square"))

let get_piece str pos =
  match get_piece_helper str pos with
  | Some k -> Piece.to_string k
  | None -> "NA"

let get_castling pos =
  match pos.castling with { wk; wq; bk; bq } -> [ wk; wq; bk; bq ]

let rank_rep = [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h' ]

(**[verify_move_string str] checks whether [str] is a valid coordinate
   representation chess move. 'e2e4' is valid, 'p1p3' is not. Ranks must
   be in "a".."h" and columns must be in 1..8. *)
let verify_move_string (str : string) =
  let length = String.length str = 4 in
  let from_rank = str.[0] in
  let valid_fr = List.exists (fun x -> x = from_rank) rank_rep in
  let from_col = Char.code str.[1] - Char.code '0' in
  let valid_fc = from_col > 0 && from_col <= 8 in
  let to_rank = str.[2] in
  let valid_tr = List.exists (fun x -> x = to_rank) rank_rep in
  let to_col = Char.code str.[3] - Char.code '0' in
  let valid_tc = to_col > 0 && to_col <= 8 in
  length && valid_fr && valid_fc && valid_tr && valid_tc

(**[get_piece_internal square pos] retrieves the piece at [rank, col] in
   [pos.board]. Requires: rank, col are within bounds for [pos.board]*)
let get_piece_internal square pos =
  match square with rank, col -> pos.board.(rank).(col)

(**[sqr_from_str] returns the square representation of the coordinate in
   [str]*)
let sqr_from_str str =
  let rank = Char.code str.[0] - Char.code 'a' in
  let col = Char.code str.[1] - Char.code '1' in
  (rank, col)

(**[find_king_sqr pos] returns the position of the king of the current
   player*)
let find_king_sqr pos color = if color then pos.wking else pos.bking

let extract_opt = function
  | Some k -> k
  | None -> raise (IllegalPiece "Cannot extract from none")

(**[verify_enemy_or_empty pos to_sqr] returns true if [to_sqr] is empty
   or occupied by an enemy piece*)
let verify_enemy_or_empty pos to_sqr =
  match get_piece_internal to_sqr pos with
  | None -> true
  | Some k -> get_turn pos <> get_color k

(**[first_piece pos rank col rankinc colinc] returns the first piece
   along the line designated by rank = rank + rankinc, col = col +
   colinc*)
let rec first_piece pos rank col rankinc colinc stationary =
  if rank > 7 || rank < 0 || col > 7 || col < 0 then None
  else if stationary = None then
    match get_piece_internal (rank, col) pos with
    | None ->
        first_piece pos (rank + rankinc) (col + colinc) rankinc colinc
          stationary
    | Some k -> Some k
  else
    match get_piece_internal (rank, col) pos with
    | None ->
        first_piece pos (rank + rankinc) (col + colinc) rankinc colinc
          stationary
    | Some k ->
        if k = extract_opt stationary then
          first_piece pos (rank + rankinc) (col + colinc) rankinc colinc
            stationary
        else Some k

(**[is_horiz_attacker piece color] returns true if [piece] attacks
   horizontally per the rules of chess*)
let is_horiz_attacker piece color =
  match piece with
  | None -> false
  | Some k -> (
      match Piece.get_piece k with
      | Rook -> get_color k = color
      | Queen -> get_color k = color
      | _ -> false )

(**[is_diag_attacker piece color] returns true if [piece] attacks
   diagonally per the rules of chess*)
let is_diag_attacker piece color =
  match piece with
  | None -> false
  | Some k -> (
      match Piece.get_piece k with
      | Bishop -> get_color k = color
      | Queen -> get_color k = color
      | _ -> false )

(**[perp_attack pos rank col color] returns true if there is a piece
   horizontally or vertically attacking the square at (rank, col) of
   [color]*)
let perp_attack pos rank col color k =
  let left_attack =
    is_horiz_attacker (first_piece pos (rank - 1) col ~-1 0 k) color
  in
  let right_attack =
    is_horiz_attacker (first_piece pos (rank + 1) col 1 0 k) color
  in
  let up_attack =
    is_horiz_attacker (first_piece pos rank (col + 1) 0 1 k) color
  in
  let down_attack =
    is_horiz_attacker (first_piece pos rank (col - 1) 0 ~-1 k) color
  in
  left_attack || right_attack || up_attack || down_attack

(**[diag_attack pos rank col color] returns true if there is a piece
   diagonally attacking the square at (rank, col) of [color]*)
let diag_attack pos rank col color k =
  let upleft =
    is_diag_attacker
      (first_piece pos (rank - 1) (col + 1) ~-1 1 k)
      color
  in
  let upright =
    is_diag_attacker (first_piece pos (rank + 1) (col + 1) 1 1 k) color
  in
  let botleft =
    is_diag_attacker
      (first_piece pos (rank - 1) (col - 1) ~-1 ~-1 k)
      color
  in
  let botright =
    is_diag_attacker
      (first_piece pos (rank + 1) (col - 1) 1 ~-1 k)
      color
  in
  upleft || upright || botleft || botright

(**[is_pawn_attacker piece color] returns true if [piece] is a pawn of
   [color]*)
let is_pawn_attacker piece color =
  match piece with
  | None -> false
  | Some k -> (
      match Piece.get_piece k with
      | Pawn -> get_color k = color
      | _ -> false )

(**[pawn_attack pos rank col color] returns true if there is a pawn
   attacking the square at (rank, col) of [color]*)
let pawn_attack pos rank col color =
  let colinc = if color then ~-1 else 1 in
  let rt_valid = if rank - 1 < 0 then false else true in
  let lft_valid = if rank + 1 > 7 then false else true in
  if col + colinc > 7 || col + colinc < 0 then false
  else
    let right =
      if rt_valid then
        is_pawn_attacker
          (get_piece_internal (rank - 1, col + colinc) pos)
          color
      else false
    in
    let left =
      if lft_valid then
        is_pawn_attacker
          (get_piece_internal (rank + 1, col + colinc) pos)
          color
      else false
    in
    right || left

(**[is_knight_attacker piece color] returns true if [piece] is a knight
   of [color]*)
let is_knight_attacker piece color =
  match piece with
  | None -> false
  | Some k -> (
      match Piece.get_piece k with
      | Knight -> get_color k = color
      | _ -> false )

(** [sqr_inbounds sqr] returns whether [sqr] is inbounds. *)
let sqr_inbounds (rank, col) =
  rank >= 0 && rank <= 7 && col >= 0 && col <= 7

(**[check_valid_sqrs pos psble_knight valid_sqr color] returns true if
   any square in [psble_knight] is [color]*)
let rec check_valid_sqrs pos psble_knight valid_sqr color =
  match (psble_knight, valid_sqr) with
  | [], [] -> false
  | kh :: kt, vh :: vt ->
      ( if vh then is_knight_attacker (get_piece_internal kh pos) color
      else false )
      || check_valid_sqrs pos kt vt color
  | _ -> false

(**[knight_attack pos rank col color] returns true if there is a knight
   attacking the square at (rank, col) of [color]*)
let knight_attack pos rank col color =
  let psble_knight =
    [
      (rank + 1, col - 2);
      (rank + 2, col - 1);
      (rank + 2, col + 1);
      (rank + 1, col + 2);
      (rank - 1, col - 2);
      (rank - 2, col - 1);
      (rank - 2, col + 1);
      (rank - 1, col + 2);
    ]
  in
  let valid_sqr = List.map sqr_inbounds psble_knight in
  check_valid_sqrs pos psble_knight valid_sqr color

(**[king_attack pos rank col color] returns true if sqr is attacked by a
   king, else false*)
let king_attack pos rank col color =
  let ksqr = if color then pos.bking else pos.wking in
  match ksqr with
  | krank, kcol ->
      if abs (krank - rank) <= 1 && abs (kcol - col) <= 1 then true
      else false

(**[attacked_square pos sqr color] returns true if [sqr] is attacked by
   a piece of [color] in [pos]*)
let attacked_square pos sqr color k =
  match sqr with
  | rank, col ->
      let perp = perp_attack pos rank col color k in
      let diag = diag_attack pos rank col color k in
      let pawn = pawn_attack pos rank col color in
      let knight = knight_attack pos rank col color in
      perp || diag || pawn || knight

(**[rook_valid_helper pos from_sqr to_sqr] verifies that the move
   (from_sqr, to_sqr) is a legal move for a rook. *)
let rook_valid_helper pos from_sqr to_sqr =
  let a = ref true in
  match (from_sqr, to_sqr) with
  | (frank, fcol), (trank, tcol) ->
      if frank = trank || fcol = tcol then
        if frank = trank && fcol < tcol then
          for i = fcol + 1 to tcol - 1 do
            match get_piece_internal (frank, i) pos with
            | None -> a := !a && true
            | Some k -> a := !a && false
          done
        else if frank = trank && tcol < fcol then
          for i = tcol + 1 to fcol - 1 do
            match get_piece_internal (frank, i) pos with
            | None -> a := !a && true
            | Some k -> a := !a && false
          done
        else if fcol = tcol && frank < trank then
          for i = frank + 1 to trank - 1 do
            match get_piece_internal (i, fcol) pos with
            | None -> a := !a && true
            | Some k -> a := !a && false
          done
        else
          for i = trank + 1 to frank - 1 do
            match get_piece_internal (i, fcol) pos with
            | None -> a := !a && true
            | Some k -> a := !a && false
          done
      else a := !a && false;
      !a

(**[bishop_valid_helper pos from_sqr to_sqr] verifies that the moves
   (from_sqr, to_sqr) is a legal move for a bishop*)
let bishop_valid_helper pos from_sqr to_sqr =
  let a = ref true in
  match (from_sqr, to_sqr) with
  | (frank, fcol), (trank, tcol) ->
      if abs (trank - frank) <> abs (tcol - fcol) then false
      else
        let b = trank - frank in
        if frank < trank then
          if fcol < tcol then
            for i = 1 to b - 1 do
              match get_piece_internal (frank + i, fcol + i) pos with
              | None -> a := !a && true
              | Some k -> a := !a && false
            done
          else
            for i = 1 to b - 1 do
              match get_piece_internal (frank + i, fcol - i) pos with
              | None -> a := !a && true
              | Some k -> a := !a && false
            done
        else if fcol > tcol then
          for i = b + 1 to -1 do
            match get_piece_internal (frank + i, fcol + i) pos with
            | None -> a := !a && true
            | Some k -> a := !a && false
          done
        else
          for i = b + 1 to -1 do
            match get_piece_internal (frank + i, fcol - i) pos with
            | None -> a := !a && true
            | Some k -> a := !a && false
          done;
        !a

(**[knight_valid_helper pos from_sqr to_sqr] ensures that the move
   (from_sqr, to_sqr) is legal for a knight and if so, executes the move
   and returns the new state. Throws: IllegalMove if the move is not
   legal for a knight *)
let knight_valid_helper pos from_sqr to_sqr =
  match (from_sqr, to_sqr) with
  | (frank, fcol), (trank, tcol) ->
      if abs (frank - trank) = 1 && abs (fcol - tcol) = 2 then true
      else if abs (frank - trank) = 2 && abs (fcol - tcol) = 1 then true
      else false

(**[queen_check_helper pos from_sqr to_sqr] verifies that the moves
   (from_sqr, to_sqr) is a legal move for a queen*)
let queen_valid_helper pos from_sqr to_sqr =
  rook_valid_helper pos from_sqr to_sqr
  || bishop_valid_helper pos from_sqr to_sqr

let is_rook x (pos : t) =
  if x <> None then
    let a = extract_opt x in
    let b = Piece.get_color a in
    let c = Piece.get_piece a in
    let d = get_turn pos in
    match c with Rook -> b == d | _ -> false
  else false

let check_castle pos frank trank =
  if get_turn pos then
    if frank > trank then
      if
        match pos.castling with
        | { wk; wq; bk; bq } ->
            wk
            && is_rook pos.board.(0).(0) pos
            && rook_valid_helper pos (frank, 0) (0, 0)
      then
        if
          (not
             (attacked_square pos (2, 0)
                (not (get_turn pos))
                (get_piece_internal (4, 0) pos)))
          && not
               (attacked_square pos (3, 0)
                  (not (get_turn pos))
                  (get_piece_internal (4, 0) pos))
        then true
        else raise (IllegalMove "King cannot castle through check")
      else raise (IllegalMove "White king cannot castle queenside")
    else if
      match pos.castling with
      | { wk; wq; bk; bq } ->
          wq
          && is_rook pos.board.(7).(0) pos
          && rook_valid_helper pos (frank, 0) (7, 0)
    then
      if
        (not
           (attacked_square pos (6, 0)
              (not (get_turn pos))
              (get_piece_internal (4, 0) pos)))
        && not
             (attacked_square pos (5, 0)
                (not (get_turn pos))
                (get_piece_internal (4, 0) pos))
      then true
      else raise (IllegalMove "King cannot castle through check")
    else raise (IllegalMove "White king cannot castle kingside")
  else if frank > trank then
    if
      match pos.castling with
      | { wk; wq; bk; bq } ->
          bq
          && is_rook pos.board.(0).(7) pos
          && rook_valid_helper pos (frank, 7) (0, 7)
    then
      if
        (not
           (attacked_square pos (2, 7)
              (not (get_turn pos))
              (get_piece_internal (4, 7) pos)))
        && not
             (attacked_square pos (3, 7)
                (not (get_turn pos))
                (get_piece_internal (4, 7) pos))
      then true
      else raise (IllegalMove "King cannot castle through check")
    else raise (IllegalMove "Black king cannot castle queenside")
  else if
    match pos.castling with
    | { wk; wq; bk; bq } ->
        bk
        && is_rook pos.board.(7).(7) pos
        && rook_valid_helper pos (frank, 7) (7, 7)
  then
    if
      (not
         (attacked_square pos (6, 7)
            (not (get_turn pos))
            (get_piece_internal (4, 7) pos)))
      && not
           (attacked_square pos (5, 7)
              (not (get_turn pos))
              (get_piece_internal (4, 7) pos))
    then true
    else raise (IllegalMove "King cannot castle through check")
  else raise (IllegalMove "Black king cannot castle kingside")

(**[king_valid_helper pos from_sqr to_sqr] verifies that the moves
   (from_sqr, to_sqr) is a legal move for a king*)
let king_valid_helper pos from_sqr to_sqr =
  let frank = fst from_sqr in
  let fcol = snd from_sqr in
  let trank = fst to_sqr in
  let tcol = snd to_sqr in
  if king_attack pos (fst to_sqr) (snd to_sqr) (get_turn pos) then
    raise (IllegalMove "King cannot move adjacent to enemy king")
  else if bishop_valid_helper pos from_sqr to_sqr then
    if abs (frank - trank) + abs (fcol - tcol) = 2 then true
    else
      raise
        (IllegalMove "King can only move one spot when not castling")
  else if rook_valid_helper pos from_sqr to_sqr then
    if abs (frank - trank + fcol - tcol) = 1 then true
    else if fcol = tcol && abs (frank - trank) = 2 && fcol mod 7 = 0
    then
      if not pos.checked then check_castle pos frank trank
      else raise (IllegalMove "King cannot castle out of check")
    else
      raise
        (IllegalMove "King can only move one spot when not castling")
  else raise (IllegalMove "King cannot move in that direction")

(**[pawn_checks pos square] returns true if the pawn on [square] checks
   the opposing player's king*)
let pawn_checks pos square =
  let ksquare = find_king_sqr pos (not (get_turn pos)) in
  match (square, ksquare) with
  | (rank, col), (krank, kcol) ->
      (krank = rank + 1 || krank = rank - 1)
      && if get_turn pos then kcol = col + 1 else kcol = col - 1

(**[knight_checks pos square] returns true if the knight on [square]
   checks the opposing player's king*)
let knight_checks pos square =
  let ksquare = find_king_sqr pos (not (get_turn pos)) in
  match (square, ksquare) with
  | (rank, col), (krank, kcol) ->
      if krank > rank then
        if kcol > col then
          (krank = rank + 2 && kcol = col + 1)
          || (krank = rank + 1 && kcol = col + 2)
        else
          (krank = rank + 2 && kcol = col - 1)
          || (krank = rank + 1 && kcol = col - 2)
      else if kcol > col then
        (krank = rank - 2 && kcol = col + 1)
        || (krank = rank - 1 && kcol = col + 2)
      else
        (krank = rank - 2 && kcol = col - 1)
        || (krank = rank - 1 && kcol = col - 2)

(**[bishop_checks pos square] returns true if the bishop on [square]
   checks the opposing player's king*)
let bishop_checks pos square =
  let ksquare = find_king_sqr pos (not (get_turn pos)) in
  match (square, ksquare) with
  | (rank, col), (krank, kcol) ->
      if abs (krank - rank) = abs (kcol - col) then
        bishop_valid_helper pos square ksquare
      else false

(**[rook_checks pos square] returns true if the rook on [square] checks
   the opposing player's king*)
let rook_checks pos square =
  let ksquare = find_king_sqr pos (not (get_turn pos)) in
  match (square, ksquare) with
  | (rank, col), (krank, kcol) ->
      if krank = rank || kcol = col then
        rook_valid_helper pos square ksquare
      else false

(**[queen_checks pos square] returns true if the queen on [square]
   checks the opposing player's king*)
let queen_checks pos square =
  let ksquare = find_king_sqr pos (not (get_turn pos)) in
  match (square, ksquare) with
  | (rank, col), (krank, kcol) ->
      if
        krank = rank || kcol = col
        || abs (krank - rank) = abs (kcol - col)
      then queen_valid_helper pos square ksquare
      else false

(**[piece_causes_check pos square] checks if the piece on [square]
   causes check for the opposing king*)
let piece_causes_check pos square =
  let piece = get_piece_internal square pos in
  match piece with
  | None -> false
  | Some k -> (
      match Piece.get_piece k with
      | Pawn -> pawn_checks pos square
      | Knight -> knight_checks pos square
      | Bishop -> bishop_checks pos square
      | Rook -> rook_checks pos square
      | Queen -> queen_checks pos square
      | King -> false )

let set_castling pos from_sqr =
  match get_piece_internal from_sqr pos with
  | None -> raise (IllegalPiece "Illegal from square")
  | Some k -> (
      match Piece.get_piece k with
      | Rook ->
          if snd from_sqr = 0 then
            if fst from_sqr = 0 then { pos.castling with wq = false }
            else if fst from_sqr = 7 then
              { pos.castling with wk = false }
            else pos.castling
          else if snd from_sqr = 7 then
            if fst from_sqr = 0 then { pos.castling with bq = false }
            else if fst from_sqr = 7 then
              { pos.castling with bk = false }
            else pos.castling
          else pos.castling
      | _ -> pos.castling )

let sqr_to_str sqr =
  match sqr with
  | a, b ->
      String.make 1 (Char.lowercase_ascii (char_of_int (a + 97)))
      ^ string_of_int (b + 1)

let convert_sqrs_to_string sqr1 sqr2 = sqr_to_str sqr1 ^ sqr_to_str sqr2

(**[rmv_piece pos sqr] removes the piece in [pos] at [sqr]. Requires:
   sqr is inbounds*)
let rmv_piece pos sqr =
  match sqr with rank, col -> pos.board.(rank).(col) <- None

(**[add_piece pos sqr piece] adds [piece] to [pos] at [sqr]. Requires:
   sqr is inbounds*)
let add_piece pos sqr piece =
  match sqr with rank, col -> pos.board.(rank).(col) <- piece

(**[mv_and_chck pos sqr] returns true if removing the piece at [sqr]
   results in the [color] king being in check, else false*)
let mv_and_chck pos from_sqr to_sqr color =
  let from_piece = get_piece_internal from_sqr pos in

  let to_piece = get_piece_internal to_sqr pos in
  rmv_piece pos from_sqr;
  add_piece pos to_sqr from_piece;
  let removing_checks =
    if
      attacked_square pos
        (if color then pos.wking else pos.bking)
        (not color) None
    then true
    else false
  in
  add_piece pos to_sqr to_piece;
  add_piece pos from_sqr from_piece;
  removing_checks

(**[causes_discovery pos from_sqr to_sqr] returns true if moving the
   piece on [from_sqr] to [to_sqr] causes check for the opposing king *)
let causes_discovery pos from_sqr to_sqr =
  mv_and_chck pos from_sqr to_sqr (get_turn pos)

let update_pieces color pieces capture promotion =
  let pieces =
    match capture with
    | None -> pieces
    | Some k ->
        if Piece.get_color k = color then
          match Piece.get_piece k with
          | Pawn -> { pieces with pawns = pieces.pawns - 1 }
          | Knight -> { pieces with knights = pieces.knights - 1 }
          | Bishop -> { pieces with bishops = pieces.bishops - 1 }
          | Rook -> { pieces with rooks = pieces.rooks - 1 }
          | Queen -> { pieces with queens = pieces.queens - 1 }
          | King -> raise (IllegalMove "Cannot capture king")
        else pieces
  in
  match promotion with
  | None -> pieces
  | Some k ->
      if Piece.get_color k = color then
        match Piece.get_piece k with
        | Knight -> { pieces with knights = pieces.knights + 1 }
        | Bishop -> { pieces with bishops = pieces.bishops + 1 }
        | Rook -> { pieces with rooks = pieces.rooks + 1 }
        | Queen -> { pieces with queens = pieces.queens + 1 }
        | Pawn -> { pieces with pawns = pieces.pawns + 1 }
        | _ -> raise (Failure "Cannot promote to king")
      else pieces

(**[add_move pos from_sqr to_sqr k] returns a new position given that
   the board array is already shifted with king position updated if [k],
   fifty_rep updated if [pawn_move] or [capture] is not None, and the
   move stack updated*)
let add_move
    pos
    (from_sqr : square)
    (to_sqr : square)
    k
    pawn_move
    promotion
    capture
    castle
    ep
    prev_castling =
  let wking = if k && pos.turn then to_sqr else pos.wking in
  let bking = if k && not pos.turn then to_sqr else pos.bking in
  let turn_check = not (get_turn pos) in
  {
    pos with
    turn = not pos.turn;
    checked =
      piece_causes_check pos to_sqr
      || causes_discovery { pos with turn = turn_check } from_sqr to_sqr;
    ep = (-1, -1);
    bking;
    wking;
    wpieces = update_pieces true pos.wpieces capture promotion;
    bpieces = update_pieces false pos.bpieces capture promotion;
    move_stack =
      (* List.rev ( (convert_sqrs_to_string from_sqr to_sqr,
         promote_str) promotion : Piece.t option; :: List.rev
         pos.move_stack ); *)
      {
        from_sqr;
        to_sqr;
        capture;
        ep_sqr = pos.ep;
        ep;
        promotion;
        castle;
        was_checked = is_in_check pos;
        halfmove_clock = pos.halfmove_clock;
        prev_castling;
      }
      :: pos.move_stack;
    fullmove_clock =
      (if pos.turn then pos.fullmove_clock else pos.fullmove_clock + 1);
    halfmove_clock =
      ( if pawn_move then 0
      else
        match capture with
        | None -> pos.halfmove_clock + 1
        | Some k -> 0 );
  }

(**[move_normal_piece pos from_sqr to_sqr] moves a piece from [from_sqr]
   to [to_sqr] taking into account whether there was a pawn move ([pm])
   or en passant ([ep])*)
let move_normal_piece pos from_sqr to_sqr pm ep promotion =
  let next_pos = { pos with castling = set_castling pos from_sqr } in
  match (from_sqr, to_sqr) with
  | (frank, fcol), (trank, tcol) ->
      let curr_piece = pos.board.(frank).(fcol) in
      let captured_piece = pos.board.(trank).(tcol) in
      pos.board.(frank).(fcol) <- None;
      pos.board.(trank).(tcol) <- curr_piece;
      add_move next_pos from_sqr to_sqr false pm None captured_piece
        false ep pos.castling

(**[move_en_passant pos from_sqr to_sqr] executes the move from_sqr
   to_sqr as an en passant move and returns the modified state.
   Precondition: The given move is a valid en passant move*)
let move_en_passant pos from_sqr to_sqr =
  let temp_pos = move_normal_piece pos from_sqr to_sqr true true None in
  match (from_sqr, to_sqr) with
  | (frank, fcol), (trank, tcol) ->
      pos.board.(trank).(fcol) <- None;
      temp_pos

(**[promote square pos piece] promotes the pawn on [square] in [pos] to
   [piece]. Requires: the piece on [square] is a pawn, [square] is
   inbounds, piece is not a king or pawn*)
let promote square pos piece =
  match square with
  | rank, col ->
      if
        match piece with
        | None -> raise (IllegalPiece "Cannot promote an empty square")
        | Some k ->
            (get_turn pos && col = 0)
            || ((not (get_turn pos)) && col = 7)
      then (
        pos.board.(rank).(col) <- piece;
        match pos.move_stack with
        | [] -> pos
        | ({ from_sqr; to_sqr; capture; ep; castle; promotion } as move)
          :: t ->
            {
              pos with
              move_stack = { move with promotion = piece } :: t;
            } )
      else pos

(**[pawn_double_move_helper pos from_sqr to_sqr] moves the pawn on
   [from_sqr] to [to_sqr] and updates the en passant square*)
let pawn_double_move_helper pos from_sqr to_sqr =
  match get_piece_internal to_sqr pos with
  | None ->
      if rook_valid_helper pos from_sqr to_sqr then
        let ep_sqr = (fst from_sqr, (snd from_sqr + snd to_sqr) / 2) in
        let next_pos =
          move_normal_piece pos from_sqr to_sqr true false None
        in
        { next_pos with ep = ep_sqr }
      else raise (IllegalMove "Illegal move for a pawn")
  | Some k -> raise (IllegalMove "Illegal move for a pawn")

(**[pawn_valid_helper pos from_sqr to_sqr] verifies that the move
   (from_sqr, to_sqr) is a legal move for a pawn and executes it,
   returning the new state. Throws: IllegalMove if the move is illegal
   Requires: from_sqr is a pawn*)
let pawn_valid_helper pos from_sqr to_sqr new_p =
  match (from_sqr, to_sqr) with
  | (frank, fcol), (trank, tcol) ->
      if
        (get_turn pos && tcol - fcol = 1)
        || ((not (get_turn pos)) && tcol - fcol = -1)
      then
        if frank <> trank then
          match get_piece_internal to_sqr pos with
          | None ->
              if to_sqr = pos.ep then
                move_en_passant pos from_sqr to_sqr
              else raise (IllegalMove "Illegal move for a pawn")
          | Some k ->
              move_normal_piece pos from_sqr to_sqr true false new_p
        else if get_piece_internal to_sqr pos = None then
          move_normal_piece pos from_sqr to_sqr true false new_p
        else raise (IllegalMove "Pawn cannot take vertically")
      else if
        (get_turn pos && tcol - fcol = 2 && fcol = 1)
        || ((not (get_turn pos)) && tcol - fcol = -2 && fcol = 6)
      then pawn_double_move_helper pos from_sqr to_sqr
      else raise (IllegalMove "Illegal move for a pawn")

let set_castles1 pos =
  if get_turn pos then { pos.castling with wk = false }
  else { pos.castling with bk = false }

let set_castles2 pos =
  if get_turn pos then { pos.castling with wq = false }
  else { pos.castling with bq = false }

(**[possibly_castle pos from_sqr to_sqr] moves the king normally or
   castles depending on the call.*)
let possibly_castle pos from_sqr to_sqr =
  let capture = get_piece_internal to_sqr pos in
  let next_pos = { pos with castling = set_castles1 pos } in
  let final_pos = { pos with castling = set_castles2 next_pos } in
  let frank = fst from_sqr in
  let trank = fst to_sqr in
  let fcol = snd from_sqr in
  let tcol = snd to_sqr in
  if fcol <> tcol || abs (frank - trank) <> 2 then (
    let curr_piece = pos.board.(frank).(fcol) in
    pos.board.(frank).(fcol) <- None;
    pos.board.(trank).(tcol) <- curr_piece;
    add_move final_pos from_sqr to_sqr true false None capture false
      false pos.castling )
  else
    let curr_piece = pos.board.(frank).(fcol) in
    if frank > trank then (
      let rook = pos.board.(0).(fcol) in
      pos.board.(frank).(fcol) <- None;
      pos.board.(trank).(tcol) <- curr_piece;
      pos.board.(0).(fcol) <- None;
      pos.board.(3).(fcol) <- rook;
      add_move final_pos from_sqr to_sqr true false None None true false
        pos.castling )
    else
      let rook = pos.board.(7).(fcol) in
      pos.board.(frank).(fcol) <- None;
      pos.board.(trank).(tcol) <- curr_piece;
      pos.board.(7).(fcol) <- None;
      pos.board.(5).(tcol) <- rook;
      add_move final_pos from_sqr to_sqr true false None None true false
        pos.castling

(**[will_be_checked pos from_sqr to_sqr] checks whether the player's
   move would put themself in check*)
let will_be_checked pos from_sqr to_sqr =
  match get_piece_internal from_sqr pos with
  | None -> raise (IllegalMove "There is no piece to move")
  | Some k -> (
      match Piece.get_piece k with
      | King ->
          attacked_square pos to_sqr
            (not (get_turn pos))
            (get_piece_internal from_sqr pos)
      | _ ->
          if attacked_square pos from_sqr (not (get_turn pos)) None then
            mv_and_chck pos from_sqr to_sqr (get_turn pos)
          else false )

(**[check_and_move piece pos from_sqr to_sqr] moves the piece [piece]
   from [from_sqr] to [to_sqr] in [pos] if it is a legal move for
   [piece], promotes the piece to [new_p] if it is a pawn, and returns
   the new state if the move is legal, else returns [pos] Precondition:
   [piece] is owned by the current player of [pos], the current player
   of [pos] is not in check*)
let check_and_move piece pos from_sqr to_sqr new_p =
  match Piece.get_piece piece with
  | Pawn -> (
      let next_pos = pawn_valid_helper pos from_sqr to_sqr new_p in
      match new_p with
      | None -> next_pos
      | Some k -> promote to_sqr next_pos new_p )
  | Knight ->
      if knight_valid_helper pos from_sqr to_sqr then
        move_normal_piece pos from_sqr to_sqr false false None
      else raise (IllegalMove "Illegal move for a knight")
  | Bishop ->
      if bishop_valid_helper pos from_sqr to_sqr then
        move_normal_piece pos from_sqr to_sqr false false None
      else raise (IllegalMove "Illegal move for a bishop")
  | Rook ->
      if rook_valid_helper pos from_sqr to_sqr then
        move_normal_piece pos from_sqr to_sqr false false None
      else raise (IllegalMove "Illegal move for a rook")
  | Queen ->
      if queen_valid_helper pos from_sqr to_sqr then
        move_normal_piece pos from_sqr to_sqr false false None
      else raise (IllegalMove "Illegal move for a queen")
  | King ->
      if king_valid_helper pos from_sqr to_sqr then
        if not (will_be_checked pos from_sqr to_sqr) then
          possibly_castle pos from_sqr to_sqr
        else raise (IllegalMove "Cannot move yourself into check")
      else raise (IllegalMove "Illegal move for a king")

(**[is_king piece] returns true if the option piece is a king,else false*)
let is_king piece =
  match Piece.get_piece piece with King -> true | _ -> false

(**[checked_move piece pos from_sqr to_sqr] moves the piece [piece] from
   [from_sqr] to [to_sqr] in [pos] if it is a legal move for [piece] and
   returns the new state if the move is legal, else returns [pos]
   Precondition: [piece] is owned by the current player of [pos], the
   current player of [pos] is in check *)
let checked_move piece pos from_sqr to_sqr new_p : t =
  let a =
    if is_king piece then not (will_be_checked pos from_sqr to_sqr)
    else
      (not (mv_and_chck pos from_sqr to_sqr (get_turn pos)))
      || not (attacked_square pos to_sqr (not (get_turn pos)) None)
  in
  if a then check_and_move piece pos from_sqr to_sqr new_p
  else raise (IllegalMove "Invalid move, you are in check!")

(**[move_helper piece pos from_sqr to_sqr] moves the piece [piece] from
   [from_sqr] to [to_sqr] if it is a legal move in [pos] and returns the
   new state if the move is legal, else returns [pos]*)
let move_helper piece pos from_sqr to_sqr new_p =
  if get_color piece = get_turn pos then
    if not (verify_enemy_or_empty pos to_sqr) then
      raise (IllegalMove "Cannot capture ally")
    else if is_in_check pos then
      checked_move piece pos from_sqr to_sqr new_p
    else if will_be_checked pos from_sqr to_sqr then
      raise (IllegalMove "Moving this piece would place you in check")
    else check_and_move piece pos from_sqr to_sqr new_p
  else
    raise
      (IllegalMove
         ( (if get_turn pos then "White" else "Black")
         ^ " does not own the piece on " ^ sqr_to_str from_sqr ))

(**[parse_promote_str str] returns the valid piece representation of
   [str]. Throws [IllegalPiece] if the string is an illegal piece *)
let parse_promote_str str pos =
  let color = get_turn pos in
  match String.uppercase_ascii str with
  | "Q" -> if color then wqueen else bqueen
  | "R" -> if color then wrook else brook
  | "N" -> if color then wknight else bknight
  | "B" -> if color then wbishop else bbishop
  | "" -> None
  | _ ->
      raise
        (IllegalPiece ("Cannot parse " ^ str ^ " as a promotable piece"))

let move str promote_str pos =
  let trm_str = String.trim str in
  if verify_move_string trm_str then
    let from_sqr = sqr_from_str (String.sub trm_str 0 2) in
    let to_sqr = sqr_from_str (String.sub trm_str 2 2) in
    let new_p = parse_promote_str promote_str pos in
    if from_sqr = to_sqr then
      raise
        (IllegalMove
           "Cannot move piece to the square it is currently at")
    else
      let piece = get_piece_internal from_sqr pos in
      match piece with
      | None ->
          raise
            (IllegalMove
               (trm_str ^ " does not contain a valid from square"))
      | Some k -> move_helper k pos from_sqr to_sqr new_p
  else
    raise
      (IllegalMove
         (trm_str ^ " is not a valid coordinate string of a move"))

let to_string pos =
  let rec to_string_helper pos rank col =
    if col < 0 then ""
    else
      let next_col = if rank = 7 then col - 1 else col in
      let next_rank = if rank = 7 then 0 else rank + 1 in
      let rank_label = Char.escaped (Char.chr (Char.code '1' + col)) in
      match get_piece_internal (rank, col) pos with
      | None ->
          " |  "
          ^ (if rank = 7 then " | " ^ rank_label ^ "\n" else "")
          ^ to_string_helper pos next_rank next_col
      | Some k ->
          " | " ^ Piece.to_string k
          ^ (if rank = 7 then " | " ^ rank_label ^ "\n" else "")
          ^ to_string_helper pos next_rank next_col
  in
  to_string_helper pos 0 7 ^ "   a   b   c   d   e   f   g   h"

(**[add_piece pos piece square] adds [piece] to [pos] at [square]*)
let add_piece pos piece square =
  match square with rank, col -> pos.board.(rank).(col) <- piece

(**[letter chr] returns true if [chr] is a lowercase or uppercase
   letter, else false*)
let letter chr =
  match chr with 'a' .. 'z' -> true | 'A' .. 'Z' -> true | _ -> false

(**[fen_parse_castling str pos] takes in the castling portion of a FEN
   string, a partially constructed position, and the index where the
   castling rights string ends and returns a boolean array of length 4
   storing the castling rights*)
let fen_parse_castling str pos =
  let castling = { wk = false; wq = false; bk = false; bq = false } in
  let rec castling_parser str castling num =
    if num = -1 then castling
    else
      match str.[num] with
      | 'K' -> castling_parser str { castling with wk = true } (num - 1)
      | 'Q' -> castling_parser str { castling with wq = true } (num - 1)
      | 'q' -> castling_parser str { castling with bq = true } (num - 1)
      | 'k' -> castling_parser str { castling with bk = true } (num - 1)
      | _ -> castling_parser str castling (num - 1)
  in
  castling_parser str castling (String.length str - 1)

(**[fen_parse_other str pos] takes in a position with board array parsed
   from [str] and adds the en passant, castling, and turn information*)
let fen_parse_other str pos =
  let turn = if str.[0] = 'w' then true else false in
  let split =
    String.split_on_char ' ' (String.sub str 2 (String.length str - 2))
  in
  match split with
  | [] -> raise (IllegalFen "FEN does not contain castling information")
  | h :: t -> (
      let castling = fen_parse_castling h pos in
      match t with
      | [] ->
          raise
            (IllegalFen "FEN does not contain en passant information")
      | h :: t ->
          let ep = if h = "-" then (-1, -1) else sqr_from_str h in
          let hlfmove =
            match t with
            | [] ->
                raise (IllegalFen "FEN does not contain halfmove clock")
            | h :: t -> int_of_string h
          in
          let fullmove =
            match t with
            | h :: h2 :: t -> int_of_string h2
            | _ ->
                raise (IllegalFen "FEN does not contain fullmove clock")
          in
          {
            pos with
            turn;
            castling;
            ep;
            checked =
              attacked_square pos
                (if turn then pos.wking else pos.bking)
                (not turn) None;
            halfmove_clock = hlfmove;
            fullmove_clock = fullmove;
          } )

let adjust_pieces pos piece =
  let pc =
    match piece with
    | None -> raise (IllegalFen "A piece was none")
    | Some k -> k
  in
  let c = Piece.get_color pc in
  let pieces =
    match Piece.get_piece pc with
    | Pawn ->
        if c then { pos.wpieces with pawns = pos.wpieces.pawns + 1 }
        else { pos.bpieces with pawns = pos.bpieces.pawns + 1 }
    | Knight ->
        if c then { pos.wpieces with knights = pos.wpieces.knights + 1 }
        else { pos.bpieces with knights = pos.bpieces.knights + 1 }
    | Bishop ->
        if c then { pos.wpieces with bishops = pos.wpieces.bishops + 1 }
        else { pos.bpieces with bishops = pos.bpieces.bishops + 1 }
    | Rook ->
        if c then { pos.wpieces with rooks = pos.wpieces.rooks + 1 }
        else { pos.bpieces with rooks = pos.bpieces.rooks + 1 }
    | Queen ->
        if c then { pos.wpieces with queens = pos.wpieces.queens + 1 }
        else { pos.bpieces with queens = pos.bpieces.queens + 1 }
    | King -> if c then pos.wpieces else pos.bpieces
  in
  if c then { pos with wpieces = pieces }
  else { pos with bpieces = pieces }

(**[fen_to_board_helper str pos rank col ind] is a recursive helper for
   turning a FEN string into a board. It returns a new position with the
   edited board and positional values. Throws: IllegalFen if the FEN
   contains illegal characters*)
let rec fen_to_board_helper str pos rank col ind =
  let nxt_ind = ind + 1 in
  if col = 0 && rank = 8 then
    fen_parse_other
      (String.sub str nxt_ind (String.length str - nxt_ind))
      pos
  else if letter str.[ind] then
    letter_fen_matching str pos rank col ind nxt_ind
  else
    match str.[ind] with
    | '/' -> fen_to_board_helper str pos 0 (col - 1) nxt_ind
    | '1' .. '7' ->
        let rankInc = Char.code str.[ind] - Char.code '0' in
        fen_to_board_helper str pos (rank + rankInc) col nxt_ind
    | '8' -> fen_to_board_helper str pos (rank + 8) col nxt_ind
    | _ ->
        raise
          (IllegalFen
             ( Char.escaped str.[ind]
             ^ " is not a valid FEN number or symbol" ))

and letter_fen_matching str pos rank col prevind nextind =
  let nk = ref (-1, -1) in
  let king_color = ref false in
  let is_king = ref false in
  let piece =
    match str.[prevind] with
    | 'p' -> bpawn
    | 'r' -> brook
    | 'n' -> bknight
    | 'b' -> bbishop
    | 'q' -> bqueen
    | 'k' ->
        is_king := true;
        king_color := false;
        nk := (rank, col);
        blking
    | 'P' -> wpawn
    | 'R' -> wrook
    | 'N' -> wknight
    | 'B' -> wbishop
    | 'Q' -> wqueen
    | 'K' ->
        is_king := true;
        king_color := true;
        nk := (rank, col);
        whking
    | _ ->
        raise
          (IllegalFen
             (Char.escaped str.[prevind] ^ " is not a valid FEN letter"))
  in
  add_piece pos piece (rank, col);
  let temp_pos = adjust_pieces pos piece in
  let new_pos =
    if !is_king then
      {
        temp_pos with
        bking = (if !king_color then pos.bking else !nk);
        wking = (if !king_color then !nk else pos.wking);
      }
    else temp_pos
  in
  fen_to_board_helper str new_pos (rank + 1) col nextind

let fen_to_board fen =
  let pos = init_empty () in
  fen_to_board_helper fen pos 0 7 0

let rec move_list moves board =
  match moves with
  | [] -> board
  | (a, b) :: k -> move_list k (move a b board)

(* let undo_prev pos = let flip = List.rev pos.move_stack in match flip
   with | h :: k -> move_list (List.rev k) (init ()) | _ -> init () *)

(**[undo_normal_move pos from_sqr to_sqr capture] reverts the move
   [from_sqr] [to_sqr]. Requires: the move did not involve castling or
   en passant *)
let undo_normal_move
    pos
    from_sqr
    to_sqr
    capture
    promotion
    was_checked
    hlf_clock
    prev_castling
    ep_sqr
    new_list =
  let pc = get_piece_internal to_sqr pos in
  pos.board.(fst from_sqr).(snd from_sqr) <- pc;
  pos.board.(fst to_sqr).(snd to_sqr) <- capture;
  let king_moved =
    match pc with
    | None ->
        raise
          (IllegalMove
             "Attempted to undo a move with a piece that wasn't there")
    | Some k -> (
        match Piece.get_piece k with King -> true | _ -> false )
  in
  let prev_turn = not (get_turn pos) in
  {
    pos with
    castling = prev_castling;
    ep = ep_sqr;
    turn = prev_turn;
    checked = was_checked;
    wking = (if prev_turn && king_moved then from_sqr else pos.wking);
    bking = (if get_turn pos && king_moved then from_sqr else pos.bking);
    wpieces = update_pieces true pos.wpieces promotion capture;
    bpieces = update_pieces false pos.bpieces promotion capture;
    move_stack = new_list;
    halfmove_clock = hlf_clock;
    fullmove_clock =
      (if prev_turn then pos.fullmove_clock else pos.fullmove_clock - 1);
  }

(**[undo_castling] undos the move [from_sqr] [to_sqr] in [pos] given
   that it was a castling move *)
let undo_castling pos from_sqr to_sqr prev_castling ep_sqr new_list =
  let prev_turn = not (get_turn pos) in
  match (from_sqr, to_sqr) with
  | (frank, fcol), (trank, tcol) ->
      (let curr_piece = pos.board.(trank).(tcol) in
       if frank > trank then (
         let rook = pos.board.(3).(fcol) in
         pos.board.(trank).(tcol) <- None;
         pos.board.(frank).(fcol) <- curr_piece;
         pos.board.(0).(fcol) <- rook;
         pos.board.(3).(fcol) <- None )
       else
         let rook = pos.board.(5).(fcol) in
         pos.board.(trank).(tcol) <- None;
         pos.board.(frank).(fcol) <- curr_piece;
         pos.board.(7).(fcol) <- rook;
         pos.board.(5).(tcol) <- None);
      {
        pos with
        castling = prev_castling;
        ep = ep_sqr;
        turn = prev_turn;
        checked = false;
        wking = (if prev_turn then from_sqr else pos.wking);
        bking = (if prev_turn then pos.bking else from_sqr);
        move_stack = new_list;
        halfmove_clock = pos.halfmove_clock - 1;
        fullmove_clock =
          ( if prev_turn then pos.fullmove_clock
          else pos.fullmove_clock - 1 );
      }

let undo_ep pos from_sqr to_sqr was_checked hlf_clock ep_sqr new_list =
  let pc = get_piece_internal to_sqr pos in
  pos.board.(fst from_sqr).(snd from_sqr) <- pc;
  pos.board.(fst to_sqr).(snd to_sqr) <- None;
  pos.board.(fst from_sqr + 1).(snd from_sqr) <-
    (if pos.turn then wpawn else bpawn);
  let prev_turn = not (get_turn pos) in
  {
    pos with
    castling = pos.castling;
    ep = ep_sqr;
    turn = prev_turn;
    checked = was_checked;
    wking = pos.wking;
    bking = pos.bking;
    move_stack = new_list;
    halfmove_clock = hlf_clock;
    fullmove_clock =
      (if prev_turn then pos.fullmove_clock else pos.fullmove_clock - 1);
  }

let undo_prev pos =
  match pos.move_stack with
  | [] -> pos
  | {
      from_sqr;
      to_sqr;
      capture;
      promotion;
      castle;
      ep;
      ep_sqr;
      was_checked;
      halfmove_clock;
      prev_castling;
    }
    :: t ->
      if castle then
        undo_castling pos from_sqr to_sqr prev_castling ep_sqr t
      else if ep then
        undo_ep pos from_sqr to_sqr was_checked halfmove_clock ep_sqr t
      else
        undo_normal_move pos from_sqr to_sqr capture promotion
          was_checked halfmove_clock prev_castling ep_sqr t

let extract pos turn =
  let a = ref [] in
  for i = 0 to turn - 2 do
    a := List.nth pos.move_stack i :: !a
  done;
  List.rev !a

let revert_prev pos turn =
  (* move_list (extract pos turn) (init ()) *)
  let rec rev_helper pos turn =
    if turn = 0 then pos else rev_helper (undo_prev pos) (turn - 1)
  in
  rev_helper pos turn

let get_turn_num pos = List.length pos.move_stack + 1

let get_piece_locs pos =
  let a = ref [] in
  for i = 0 to 7 do
    for j = 0 to 7 do
      match get_piece_internal (7 - i, 7 - j) pos with
      | Some k ->
          if get_color k = pos.turn then a := (7 - i, 7 - j) :: !a
      | None -> a := !a
    done
  done;
  !a

let in_range x = fst x > -1 && fst x < 8 && snd x > -1 && snd x < 8

let avail_move_pawn_one piece pos checked =
  let b = if pos.turn then 1 else -1 in
  let g = (fst piece, snd piece + b) in
  if will_be_checked pos piece g then ""
  else if
    if checked then
      (not (mv_and_chck pos piece g (get_turn pos)))
      && not (attacked_square pos piece (not (get_turn pos)) None)
    else true
  then
    match get_piece_internal g pos with
    | None -> sqr_to_str piece ^ sqr_to_str g
    | Some k -> ""
  else ""

let avail_move_pawn_two piece pos checked =
  if (pos.turn && snd piece = 1) || ((not pos.turn) && snd piece = 6)
  then
    let x = if pos.turn then 2 else -2 in
    let g = (fst piece, snd piece + x) in
    if
      rook_valid_helper pos piece g
      && get_piece_internal g pos = None
      && not (will_be_checked pos piece g)
    then
      if
        if checked then
          (not (mv_and_chck pos piece g (get_turn pos)))
      && not (attacked_square pos piece (not (get_turn pos)) None)
        else true
      then sqr_to_str piece ^ sqr_to_str (fst piece, snd piece + x)
      else ""
    else ""
  else ""

let avail_move_pawn_diag piece pos x checked =
  let b = if pos.turn then 1 else -1 in
  let c = if x then 1 else -1 in
  let g = (fst piece + c, snd piece + b) in
  if
    in_range g
    && (not (will_be_checked pos piece g))
    && verify_enemy_or_empty pos g
  then
    if
      if checked then
        (not (mv_and_chck pos piece g (get_turn pos)))
      && not (attacked_square pos piece (not (get_turn pos)) None)
      else true
    then
      match get_piece_internal g pos with
      | Some k -> sqr_to_str piece ^ sqr_to_str g
      | None ->
          if g = pos.ep then sqr_to_str piece ^ sqr_to_str g else ""
    else ""
  else ""

let avail_move_pawn_general piece pos checked =
  [
    avail_move_pawn_two piece pos checked;
    avail_move_pawn_diag piece pos true checked;
    avail_move_pawn_diag piece pos false checked;
    avail_move_pawn_one piece pos checked;
  ]

let avail_move_diag piece pos x y checked =
  let a = ref [] in
  for i = 1 to 7 do
    let g =
      ( (fst piece + if x then i else -i),
        snd piece + if y then i else -i )
    in
    if
      not
        (in_range g
        && bishop_valid_helper pos piece g
        && (not (will_be_checked pos piece g))
        && verify_enemy_or_empty pos g )
    then a := !a
    else if
      if checked then
        (not (mv_and_chck pos piece g (get_turn pos)))
      && not (attacked_square pos piece (not (get_turn pos)) None)
      else true
    then a := (sqr_to_str piece ^ sqr_to_str g) :: !a
    else a := !a
  done;
  !a

let avail_move_bishop piece pos checked =
  avail_move_diag piece pos true true checked
  @ avail_move_diag piece pos true false checked
  @ avail_move_diag piece pos false false checked
  @ avail_move_diag piece pos false true checked

let avail_knight piece pos x y checked =
  let g = (fst piece + x, snd piece + y) in
  if
    not
      ( in_range g
      && verify_enemy_or_empty pos g
      && not (will_be_checked pos piece g) )
  then ""
  else if
    if checked then
      (not (mv_and_chck pos piece g (get_turn pos)))
      && not (attacked_square pos piece (not (get_turn pos)) None)
    else true
  then sqr_to_str piece ^ sqr_to_str g
  else ""

let avail_move_knight piece pos checked =
  [
    avail_knight piece pos 2 1 checked;
    avail_knight piece pos 2 (-1) checked;
    avail_knight piece pos 1 2 checked;
    avail_knight piece pos 1 (-2) checked;
    avail_knight piece pos (-2) 1 checked;
    avail_knight piece pos (-2) (-1) checked;
    avail_knight piece pos (-1) (-2) checked;
    avail_knight piece pos (-1) 2 checked;
  ]

let avail_castles piece pos c =
  if pos.turn && piece = (4, 0) then
    if
      get_piece_internal (4 + (c / 2), 0) pos = None
      && get_piece_internal (4 + c, 0) pos = None
      && (not (will_be_checked pos (4, 0) (4 + c, 0)))
      && (try check_castle pos 4 (4 + c) with exn -> false)
      && verify_enemy_or_empty pos (4 + c, 0)
    then sqr_to_str (4, 0) ^ sqr_to_str (4 + c, 0)
    else ""
  else if (not pos.turn) && piece = (4, 7) then
    if
      get_piece_internal (4 + (c / 2), 7) pos = None
      && get_piece_internal (4 + c, 7) pos = None
      && (not (will_be_checked pos (4, 7) (4 + c, 7)))
      && (try check_castle pos 4 (4 + c) with exn -> false)
      && verify_enemy_or_empty pos (4 + c, 7)
    then sqr_to_str (4, 7) ^ sqr_to_str (4 + c, 7)
    else ""
  else ""

let avail_move_king piece pos =
  let a = ref [] in
  for i = -1 to 1 do
    for j = -1 to 1 do
      let g = (fst piece + i, snd piece + j) in
      try
        if
          (not (will_be_checked pos piece g))
          && king_valid_helper pos piece g
          && in_range g
          && verify_enemy_or_empty pos g
        then a := (sqr_to_str piece ^ sqr_to_str g) :: !a
      with exn -> a := !a
    done
  done;
  if not pos.checked then
    avail_castles piece pos 2 :: avail_castles piece pos (-2) :: !a
  else !a

(** [avail_move_aux piece pos x checked dirxn] finds available moves for
    [piece] on board [pos], and checks vertical moves if [dirxn] is
    [true] or horizontal moves if it is [false]. *)
let avail_move_aux piece pos x checked dirxn =
  let a = ref [] in
  for i = 1 to 7 do
    let g =
      if dirxn then (fst piece, snd piece + if x then i else -i)
      else ((fst piece + if x then i else -i), snd piece)
    in
    if
      not
        ( in_range g
        && (not (will_be_checked pos piece g))
        && rook_valid_helper pos piece g
        && verify_enemy_or_empty pos g )
    then a := !a
    else if checked then
      if
        (not (mv_and_chck pos piece g (get_turn pos)))
      && not (attacked_square pos piece (not (get_turn pos)) None)
      then a := (sqr_to_str piece ^ sqr_to_str g) :: !a
      else a := !a
    else a := (sqr_to_str piece ^ sqr_to_str g) :: !a
  done;
  !a

let avail_move_vert piece pos x checked =
  avail_move_aux piece pos x checked true

let avail_move_horiz piece pos x checked =
  avail_move_aux piece pos x checked false

let avail_move_rook piece pos checked =
  avail_move_horiz piece pos true checked
  @ avail_move_horiz piece pos false checked
  @ avail_move_vert piece pos true checked
  @ avail_move_vert piece pos false checked

let avail_move piece pos checked =
  match
    Piece.get_piece (extract_opt (get_piece_internal piece pos))
  with
  | Pawn -> avail_move_pawn_general piece pos checked
  | Bishop -> avail_move_bishop piece pos checked
  | Rook -> avail_move_rook piece pos checked
  | Queen ->
      avail_move_bishop piece pos checked
      @ avail_move_rook piece pos checked
  | Knight -> avail_move_knight piece pos checked
  | King -> avail_move_king piece pos

let rec avail_moves piece_list pos checked =
  match piece_list with
  | h :: t -> avail_move h pos checked :: avail_moves t pos checked
  | [] -> []

let move_generator pos =
  pos.checked
  |> avail_moves (get_piece_locs pos) pos
  |> List.flatten
  |> List.filter (fun x -> x <> "")

let build_boardstring board =
  let acc = Buffer.create 64 in
  for i = 0 to 7 do
    let inc = ref 0 in
    for j = 0 to 7 do
      match board.(j).(7 - i) with
      | None -> inc := !inc + 1
      | Some k ->
          let tinc = !inc in
          inc := 0;
          if tinc > 0 then Buffer.add_string acc (string_of_int tinc);
          Buffer.add_string acc (Piece.to_string k)
    done;
    if !inc > 0 then Buffer.add_string acc (string_of_int !inc);
    if i < 7 then Buffer.add_string acc "/"
  done;
  Buffer.contents acc

let map_castling_str pos =
  let temp = Buffer.create 4 in
  match pos.castling with
  | { wk; wq; bk; bq } ->
      if wk then Buffer.add_char temp 'K';
      if wq then Buffer.add_char temp 'Q';
      if bk then Buffer.add_char temp 'k';
      if bq then Buffer.add_char temp 'q';
      Buffer.contents temp

let to_fen_no_mvclck pos =
  let str = Buffer.create 80 in
  Buffer.add_string str (build_boardstring pos.board);
  Buffer.add_string str " ";
  Buffer.add_string str (if get_turn pos then "w" else "b");
  Buffer.add_string str " ";
  Buffer.add_string str
    (let cstl_str = map_castling_str pos in
     if cstl_str = "" then "-" else cstl_str);
  Buffer.add_string str " ";
  Buffer.add_string str
    (if pos.ep <> (-1, -1) then sqr_to_str pos.ep else "-");
  Buffer.contents str

let to_fen pos =
  let str = Buffer.create 80 in
  Buffer.add_string str (build_boardstring pos.board);
  Buffer.add_string str " ";
  Buffer.add_string str (if get_turn pos then "w" else "b");
  Buffer.add_string str " ";
  Buffer.add_string str
    (let cstl_str = map_castling_str pos in
     if cstl_str = "" then "-" else cstl_str);
  Buffer.add_string str " ";
  Buffer.add_string str
    (if pos.ep <> (-1, -1) then sqr_to_str pos.ep else "-");
  Buffer.add_string str " ";
  Buffer.add_string str (string_of_int pos.halfmove_clock);
  Buffer.add_string str " ";
  Buffer.add_string str (string_of_int pos.fullmove_clock);
  Buffer.contents str

let equals pos1 pos2 =
  let castling = pos1.castling = pos2.castling in
  let board = pos1.board = pos2.board in
  let ep = pos1.ep = pos2.ep in
  let turn = pos1.turn = pos2.turn in
  let checked = pos1.checked = pos2.checked in
  let wking = pos1.wking = pos2.wking in
  let bking = pos1.bking = pos2.bking in
  let halfmove_clock = pos1.halfmove_clock = pos2.halfmove_clock in
  let wpieces = pos1.wpieces = pos2.wpieces in
  let bpieces = pos1.bpieces = pos2.bpieces in
  castling && board && ep && turn && checked && wking && bking
  && halfmove_clock && wpieces && bpieces

let check_kings pos =
  match get_piece_locs pos with
  | [ t ] -> pos.wking = t || pos.bking = t
  | _ -> false

let checkmate pos =
  (check_kings pos && check_kings { pos with turn = not pos.turn })
  || List.length (move_generator pos) <= 0

let mv_to_str move =
  ( sqr_to_str move.from_sqr ^ sqr_to_str move.to_sqr,
    match move.promotion with None -> "" | Some k -> Piece.to_string k
  )

(**[threefold_repetition pos] returns true if the current player of
   [pos] can claim a draw by threefold repetition *)
let threefold_repetition pos =
  let mv_stck = ref [] in
  let compare_fen = to_fen_no_mvclck pos in
  let rec three_rep_helper pos current_pos inc =
    match current_pos.move_stack with
    | [] -> false
    | h :: t ->
        if match h.capture with None -> true | Some k -> false then (
          mv_stck := mv_to_str h :: !mv_stck;
          let next_pos = undo_prev current_pos in
          if to_fen_no_mvclck next_pos = compare_fen then
            if inc + 1 = 3 then (
              move_list !mv_stck next_pos;
              true )
            else three_rep_helper pos next_pos (inc + 1)
          else three_rep_helper pos next_pos inc )
        else (
          move_list !mv_stck current_pos;
          false )
  in
  three_rep_helper compare_fen pos 1

(**[fiftyfold_rule pos] returns true if the current player can claim a
   draw by the fiftyfold rule *)
let fiftyfold_rule pos = pos.halfmove_clock = 100

(**[insufficient_material pos] returns true if the game is drawn by
   insufficient material *)
let insufficient_material pos =
  if
    pos.wpieces.queens = 0 && pos.bpieces.queens = 0
    && pos.wpieces.rooks = 0 && pos.bpieces.rooks = 0
    && pos.wpieces.pawns = 0 && pos.bpieces.pawns = 0
  then
    match (pos.wpieces, pos.bpieces) with
    | { knights = 0; bishops = 1 }, { knights = 0; bishops = 1 }
    | { knights = 0; bishops = 0 }, { knights = 0; bishops = 1 }
    | { knights = 0; bishops = 1 }, { knights = 0; bishops = 0 }
    | { knights = 1; bishops = 0 }, { knights = 1; bishops = 0 }
    | { knights = 1; bishops = 0 }, { knights = 0; bishops = 1 }
    | { knights = 0; bishops = 1 }, { knights = 1; bishops = 0 }
    | { knights = 0; bishops = 0 }, { knights = 1; bishops = 0 }
    | { knights = 1; bishops = 0 }, { knights = 0; bishops = 0 }
    | { knights = 0; bishops = 0 }, { knights = 0; bishops = 0 } ->
        true
    | _ -> false
  else false

let draw pos =
  threefold_repetition pos
  || fiftyfold_rule pos
  || insufficient_material pos

let eval_move pos = failwith "unimplemented"

let get_moves pos =
  let rec gm_helper move_stack acc =
    match move_stack with
    | [] -> acc
    | { from_sqr; to_sqr; promotion } :: t ->
        let promote_str =
          match promotion with
          | None -> ""
          | Some k -> Piece.to_string k
        in
        gm_helper t
          ((convert_sqrs_to_string from_sqr to_sqr, promote_str) :: acc)
  in
  gm_helper pos.move_stack []
