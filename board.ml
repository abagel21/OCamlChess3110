open Piece

(**TODO 1. implement piece to_string 2. Implement pawn movement
   (including en passant) Alex 4. implement pins (if a move causes the
   player to be in check) 6. Finish FEN function Alex 7. Testing board
   8. Command Line 9. Move under check 5. Implement checkmate/draw
   checking*)

(*AF: the record {board, castling, ep, turn} represents a full chess
  position where board=[|[|a1...a8|];...;[|h1...h8|]|] represents the
  squares of the board, castling=[qw;kw;qb;kb] such that qw is a boolean
  representing whether white has queenside castling and the rest follow,
  ep=(int, int) where the first int is the row and the second int is the
  column of the en passant square, turn=bool where true is white's turn
  and false is black's turn the string list of type r represents the
  move stack of the position, prev=t represents the previous position
  state*)
(*RI=The t.board array is always full*)

type r = Piece.t option array array

exception IllegalSquare of string

exception IllegalMove of string

exception IllegalFen of string

exception IllegalPiece

exception EmptyMoveStack

type square = int * int

type move = {
  from_sqr : square;
  to_sqr : square;
  en_passant : bool;
  castling : bool;
  promote : bool;
}

type t = {
  board : r;
  castling : bool array;
  ep : int * int;
  turn : bool;
  move_stack : move list;
  checked : bool;
  wking : square;
  bking : square;
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
let init_empty_board_array = Array.make_matrix 8 8 None

(**[init_empty] returns a board representation that is empty *)
let init_empty () =
  {
    board = init_empty_board_array;
    castling = [| false; false; false; false |];
    ep = (-1, -1);
    turn = true;
    move_stack = [];
    checked = false;
    wking = (-1, -1);
    bking = (-1, -1);
  }

let init () =
  {
    board = Array.of_list (List.map Array.of_list init_board_list);
    castling = [| true; true; true; true |];
    ep = (-1, -1);
    turn = true;
    move_stack = [];
    checked = false;
    wking = (0, 4);
    bking = (7, 4);
  }

let get_turn pos = pos.turn

let get_piece str pos =
  let trm_str = String.trim str in
  if String.length trm_str = 2 then
    let rank = Char.code str.[0] - Char.code 'a' in
    let col = Char.code str.[1] - Char.code '1' in
    if rank > 7 || rank < 0 || col > 7 || rank < 0 then
      raise (IllegalSquare (str ^ " is not a valid square"))
    else pos.board.(rank).(col)
  else raise (IllegalSquare (str ^ " is not a legal square"))

let rank_rep = [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h' ]

(**[verify_move_string str] checks whether [str] is a valid coordinate
   representation chess move. 'e2e4' is valid, 'p1p3' is not. Ranks must
   be in {a-h} and columns must be in {1-8}*)
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

let sqr_from_str str =
  let rank = Char.code str.[0] - Char.code 'a' in
  let col = Char.code str.[1] - Char.code '1' in
  (rank, col)

(**[is_check pos] returns true if the player [get_turn pos] is in check,
   else false*)
let is_check pos = pos.checked

(**[find_king_sqr pos] returns the position of the king of the current
   player*)
let find_king_sqr pos color = if color then pos.wking else pos.bking

let verify_enemy_or_empty pos to_sqr =
  match get_piece_internal to_sqr pos with
  | None -> true
  | Some k -> get_turn pos <> get_color k

(**[first_piece pos rank col rankinc colinc] returns the first piece
   along the line designated by rank = rank + rankinc, col = col +
   colinc*)
let rec first_piece pos rank col rankinc colinc =
  if rank > 7 || rank < 0 || col > 7 || col < 0 then None
  else
    match get_piece_internal (rank, col) pos with
    | None ->
        first_piece pos (rank + rankinc) (col + colinc) rankinc colinc
    | Some k -> Some k

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
let perp_attack pos rank col color =
  let left_attack =
    is_horiz_attacker
      (first_piece pos rank col ~-1 0)
      (not (get_turn pos))
  in
  let right_attack =
    is_horiz_attacker
      (first_piece pos rank col 1 0)
      (not (get_turn pos))
  in
  let up_attack =
    is_horiz_attacker
      (first_piece pos rank col 0 1)
      (not (get_turn pos))
  in
  let down_attack =
    is_horiz_attacker
      (first_piece pos rank col 0 ~-1)
      (not (get_turn pos))
  in
  left_attack || right_attack || up_attack || down_attack

(**[diag_attack pos rank col color] returns true if there is a piece
   diagonally attacking the square at (rank, col) of [color]*)
let diag_attack pos rank col color =
  let upleft =
    is_diag_attacker (first_piece pos rank col 1 1) (not (get_turn pos))
  in
  let upright =
    is_diag_attacker
      (first_piece pos rank col ~-1 1)
      (not (get_turn pos))
  in
  let botleft =
    is_diag_attacker
      (first_piece pos rank col ~-1 ~-1)
      (not (get_turn pos))
  in
  let botright =
    is_diag_attacker
      (first_piece pos rank col 1 ~-1)
      (not (get_turn pos))
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

(**[diag_attack pos rank col color] returns true if there is a pawn
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

(**[sqr_inbounds sqr] returns true if [sqr] is inbounds, else false*)
let sqr_inbounds sqr =
  match sqr with
  | rank, col -> rank >= 0 && rank <= 7 && col >= 0 && col <= 7

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

(**[diag_attack pos rank col color] returns true if there is a knight
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

(**[attacked_square pos sqr color] returns true if [sqr] is attacked by
   a piece of [color] in [pos]*)
let attacked_square pos sqr color =
  match sqr with
  | rank, col ->
      let perp = perp_attack pos rank col color in
      let diag = diag_attack pos rank col color in
      let pawn = pawn_attack pos rank col color in
      let knight = knight_attack pos rank col color in
      perp || diag || pawn || knight

let checked_move piece pos from_sqr to_sqr =
  match Piece.get_piece piece with
  | Pawn -> failwith "Unimplemented"
  | Knight -> failwith "Unimplemented"
  | Bishop -> failwith "Unimplemented"
  | Rook -> failwith "Unimplemented"
  | Queen -> failwith "Unimplemented"
  | King -> failwith "Unimplemented"

(**[rook_valid_helper pos from_sqr to_sqr] verifies that the move
   (from_sqr, to_sqr) is a legal move for a rook. *)
let rook_valid_helper pos from_sqr to_sqr =
  if not (verify_enemy_or_empty pos to_sqr) then
    raise (IllegalMove "Cannot capture ally")
  else
    let a = ref true in
    match (from_sqr, to_sqr) with
    | (frank, fcol), (trank, tcol) ->
        if frank = trank then
          for i = fcol + 1 to tcol - 1 do
            match get_piece_internal (frank, i) pos with
            | None -> a := !a && true
            | Some k -> a := !a && false
          done
        else if fcol = tcol then
          for i = frank + 1 to trank - 1 do
            match get_piece_internal (i, fcol) pos with
            | None -> a := !a && true
            | Some k -> a := !a && false
          done;
        !a

(**[bishop_valid_helper pos from_sqr to_sqr] verifies that the moves
   (from_sqr, to_sqr) is a legal move for a bishop*)
let bishop_valid_helper pos from_sqr to_sqr =
  if not (verify_enemy_or_empty pos to_sqr) then
    raise (IllegalMove "Cannot capture ally")
  else
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
  if not (verify_enemy_or_empty pos to_sqr) then
    raise (IllegalMove "Cannot capture ally")
  else
    match (from_sqr, to_sqr) with
    | (frank, fcol), (trank, tcol) ->
        if abs (frank - trank) = 1 && abs (fcol - tcol) = 2 then true
        else if abs (frank - trank) = 2 && abs (fcol - tcol) = 1 then
          true
        else raise (IllegalMove "Illegal move for knight")

(**[queen_check_helper pos from_sqr to_sqr] verifies that the moves
   (from_sqr, to_sqr) is a legal move for a queen*)
let queen_valid_helper pos from_sqr to_sqr =
  rook_valid_helper pos from_sqr to_sqr
  || bishop_valid_helper pos from_sqr to_sqr

let check_castle pos fcol tcol =
  if get_turn pos then
    if fcol > tcol then
      if pos.castling.(0) then true
      else raise (IllegalMove "King cannot castle here")
    else if pos.castling.(1) then true
    else raise (IllegalMove "King cannot castle here")
  else if fcol > tcol then
    if pos.castling.(2) then true
    else raise (IllegalMove "King cannot castle here")
  else if pos.castling.(3) then true
  else raise (IllegalMove "King cannot castle here")

(**[king_valid_helper pos from_sqr to_sqr] verifies that the moves
   (from_sqr, to_sqr) is a legal move for a king*)
let king_valid_helper pos from_sqr to_sqr =
  let frank = fst from_sqr in
  let fcol = snd from_sqr in
  let trank = fst to_sqr in
  let tcol = fst to_sqr in
  if bishop_valid_helper pos from_sqr to_sqr then
    if abs (frank - trank + fcol - tcol) = 2 then true
    else raise (IllegalMove "King can only move on spot diagonally")
  else if rook_valid_helper pos from_sqr to_sqr then
    if abs (frank - trank + fcol - tcol) = 1 then true
    else if frank = trank && abs (fcol - tcol) = 2 then
      check_castle pos fcol tcol
    else false
  else raise (IllegalMove "King cannot move in that direction")

(**[pawn_checks pos square] returns true if the pawn on [square] checks
   the opposing player's king*)
let pawn_checks pos square =
  let ksquare = find_king_sqr pos (get_turn pos) in
  match (square, ksquare) with
  | (rank, col), (krank, kcol) ->
      krank = rank + 1 && (krank = col - 1 || krank = col + 1)

(**[knight_checks pos square] returns true if the knight on [square]
   checks the opposing player's king*)
let knight_checks pos square =
  let ksquare = find_king_sqr pos (get_turn pos) in
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
  let ksquare = find_king_sqr pos (get_turn pos) in
  match (square, ksquare) with
  | (rank, col), (krank, kcol) ->
      if abs (krank - rank) = abs (kcol - col) then
        bishop_valid_helper pos square ksquare
      else false

(**[rook_checks pos square] returns true if the rook on [square] checks
   the opposing player's king*)
let rook_checks pos square =
  let ksquare = find_king_sqr pos (get_turn pos) in
  match (square, ksquare) with
  | (rank, col), (krank, kcol) ->
      if krank = rank || kcol = col then
        rook_valid_helper pos square ksquare
      else false

(**[queen_checks pos square] returns true if the queen on [square]
   checks the opposing player's king*)
let queen_checks pos square =
  let ksquare = find_king_sqr pos (get_turn pos) in
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
  | None -> raise IllegalPiece
  | Some k -> (
      match Piece.get_piece k with
      | Rook ->
          if fst from_sqr = 0 then
            if snd from_sqr = 0 then pos.castling.(0) <- false
            else if snd from_sqr = 7 then pos.castling.(1) <- false
            else pos.castling.(0) <- pos.castling.(0)
          else if fst from_sqr = 7 then
            if snd from_sqr = 0 then pos.castling.(2) <- false
            else if snd from_sqr = 7 then pos.castling.(3) <- false
            else pos.castling.(0) <- pos.castling.(0)
          else pos.castling.(0) <- pos.castling.(0)
      | _ -> pos.castling.(0) <- pos.castling.(0) )

(**[is_castling pos from_sqr to_sqr k] returns true if the move is a
   castling move, else false*)
let is_castling pos from_sqr to_sqr k =
  match (from_sqr, to_sqr) with
  | (_, fcol), (_, tcol) ->
      if k && abs (fcol - tcol) > 1 then true else false

(**[add_move pos from_sqr to_sqr k] returns a new position given that
   the board array is already shifted*)
let add_move pos (from_sqr : square) (to_sqr : square) k =
  let wking = if k && pos.turn then to_sqr else pos.wking in
  let bking = if not pos.turn then to_sqr else pos.bking in
  {
    pos with
    turn = not pos.turn;
    move_stack =
      {
        from_sqr;
        to_sqr;
        en_passant = false;
        promote = false;
        castling = is_castling pos from_sqr to_sqr k;
      }
      :: pos.move_stack;
    checked = piece_causes_check pos to_sqr;
    bking;
    wking;
  }

(**[move_normal_piece pos from_sqr to_sqr] moves a piece from [from_sqr]
   to [to_sqr]*)
let move_normal_piece pos from_sqr to_sqr =
  set_castling pos from_sqr;
  match (from_sqr, to_sqr) with
  | (frank, fcol), (trank, tcol) ->
      let curr_piece = pos.board.(frank).(fcol) in
      pos.board.(frank).(fcol) <- None;
      pos.board.(trank).(tcol) <- curr_piece;
      add_move pos from_sqr to_sqr false

(**[move_en_passant pos from_sqr to_sqr] executes the move from_sqr
   to_sqr as an en passant move and returns the modified state.
   Precondition: The given move is a valid en passant move*)
let move_en_passant pos from_sqr to_sqr =
  let temp_pos = move_normal_piece pos from_sqr to_sqr in
  let adj = if get_turn pos then ~-1 else 1 in
  match (from_sqr, to_sqr) with
  | (frank, fcol), (trank, tcol) ->
      pos.board.(trank + adj).(tcol) <- None;
      temp_pos

(**[promote square pos piece] promotes the piece on [square] in [pos] to
   [piece]*)
let promote square pos piece =
  match square with
  | rank, col ->
      pos.board.(rank).(col) <- piece;
      pos

(**[pawn_double_move_helper pos from_sqr to_sqr]*)
let pawn_double_move_helper pos from_sqr to_sqr =
  match get_piece_internal to_sqr pos with
  | None ->
      if rook_valid_helper pos from_sqr to_sqr then
        move_normal_piece pos from_sqr to_sqr
      else raise (IllegalMove "Illegal move for a pawn")
  | Some k -> raise (IllegalMove "Illegal move for a pawn")

(**[pawn_valid_helper pos from_sqr to_sqr] verifies that the move
   (from_sqr, to_sqr) is a legal move for a pawn and executes it,
   returning the new state. Throws: IllegalMove if the move is illegal
   Requires: from_sqr is a pawn*)
let pawn_valid_helper pos from_sqr to_sqr new_p =
  match (from_sqr, to_sqr) with
  | (frank, fcol), (trank, tcol) ->
      let next_pos =
        if
          (get_turn pos && abs (tcol - fcol) = 1)
          || ((not (get_turn pos)) && tcol - fcol = -1)
        then
          if fcol <> tcol && to_sqr = pos.ep then
            match get_piece_internal to_sqr pos with
            | None -> move_en_passant pos from_sqr to_sqr
            | Some k -> move_normal_piece pos from_sqr to_sqr
          else if frank = trank then
            move_normal_piece pos from_sqr to_sqr
          else raise (IllegalMove "Illegal move for a pawn")
        else if
          (get_turn pos && tcol - fcol = 2 && fcol = 1)
          || ((not (get_turn pos)) && tcol - fcol = -2 && fcol = 6)
        then pawn_double_move_helper pos from_sqr to_sqr
        else raise (IllegalMove "Illegal move for a pawn")
      in
      promote to_sqr next_pos new_p

(**[is_check pos] returns true if the player [get_turn pos] is in check,
   else false*)
let is_check pos = pos.checked

let set_castles1 pos =
  if get_turn pos then pos.castling.(0) <- false
  else pos.castling.(2) <- false

let set_castles2 pos =
  if get_turn pos then pos.castling.(1) <- false
  else pos.castling.(3) <- false

(**[possibly_castle pos from_sqr to_sqr] moves the king normally or
   castles dependinng on the call.*)
let possibly_castle pos from_sqr to_sqr =
  set_castles1 pos;
  set_castles2 pos;
  let frank = fst from_sqr in
  let trank = fst to_sqr in
  let fcol = snd from_sqr in
  let tcol = snd to_sqr in
  if frank <> trank || abs (fcol - tcol) <> 2 then (
    let curr_piece = pos.board.(frank).(fcol) in
    pos.board.(frank).(fcol) <- None;
    pos.board.(trank).(tcol) <- curr_piece;
    add_move pos from_sqr to_sqr true )
  else
    let curr_piece = pos.board.(frank).(fcol) in
    if fcol > tcol then (
      let rook = pos.board.(frank).(0) in
      pos.board.(frank).(fcol) <- None;
      pos.board.(trank).(tcol) <- curr_piece;
      pos.board.(frank).(0) <- None;
      pos.board.(frank).(3) <- rook;
      add_move pos from_sqr to_sqr true )
    else
      let rook = pos.board.(frank).(7) in
      pos.board.(frank).(fcol) <- None;
      pos.board.(trank).(tcol) <- curr_piece;
      pos.board.(frank).(7) <- None;
      pos.board.(frank).(5) <- rook;
      add_move pos from_sqr to_sqr true

(**[will_be_checked pos from_sqr to_sqr] checks whether the player's
   move would put themself in check*)
let will_be_checked pos from_sqr to_sqr =
  let king = find_king_sqr pos (get_turn pos) in
  match (king, from_sqr) with
  | (krank, kcol), (frank, fcol) ->
      if krank - frank = kcol - fcol then failwith "unimplemented"
      else if true then failwith ""
      else failwith ""

(**[checked_move piece pos from_sqr to_sqr] moves the piece [piece] from
   [from_sqr] to [to_sqr] in [pos] if it is a legal move for [piece] and
   returns the new state if the move is legal, else returns [pos]
   Precondition: [piece] is owned by the current player of [pos], the
   current player of [pos] is in check *)
let checked_move piece pos from_sqr to_sqr : t =
  match Piece.get_piece piece with
  | Pawn -> failwith ""
  | Knight -> failwith ""
  | Bishop -> failwith ""
  | Rook -> failwith ""
  | Queen -> failwith ""
  | King -> failwith ""

(**[check_and_move piece pos from_sqr to_sqr] moves the piece [piece]
   from [from_sqr] to [to_sqr] in [pos] if it is a legal move for
   [piece], promotes the piece to [new_p] if it is a pawn, and returns
   the new state if the move is legal, else returns [pos] Precondition:
   [piece] is owned by the current player of [pos], the current player
   of [pos] is not in check*)
let check_and_move piece pos from_sqr to_sqr new_p =
  match Piece.get_piece piece with
  | Pawn -> pawn_valid_helper pos from_sqr to_sqr new_p
  | Knight ->
      if knight_valid_helper pos from_sqr to_sqr then
        move_normal_piece pos from_sqr to_sqr
      else pos
  | Bishop ->
      if bishop_valid_helper pos from_sqr to_sqr then
        move_normal_piece pos from_sqr to_sqr
      else pos
  | Rook ->
      if rook_valid_helper pos from_sqr to_sqr then
        move_normal_piece pos from_sqr to_sqr
      else pos
  | Queen ->
      if queen_valid_helper pos from_sqr to_sqr then
        move_normal_piece pos from_sqr to_sqr
      else pos
  | King ->
      if king_valid_helper pos from_sqr to_sqr then
        possibly_castle pos from_sqr to_sqr
      else pos

(**[move_helper piece pos from_sqr to_sqr] moves the piece [piece] from
   [from_sqr] to [to_sqr] if it is a legal move in [pos] and returns the
   new state if the move is legal, else returns [pos]*)
let move_helper piece pos from_sqr to_sqr new_p =
  if get_color piece = get_turn pos then
    if is_check pos then checked_move piece pos from_sqr to_sqr
    else check_and_move piece pos from_sqr to_sqr new_p
  else
    raise
      (IllegalMove
         ( (if get_turn pos then "White" else "Black")
         ^ " does not own this piece" ))

(**[parse_promote_str str] returns the valid piece representation of
   [str]. Throws [IllegalPiece] if the string is an illegal piece *)
let parse_promote_str str pos =
  let color = get_turn pos in
  match str with
  | "Q" -> if color then wqueen else bqueen
  | "R" -> if color then wrook else brook
  | "N" -> if color then wknight else bknight
  | "B" -> if color then wbishop else bbishop
  | "" -> if color then wpawn else bpawn
  | _ -> raise IllegalPiece

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

let undo_prev pos = failwith "Unimplemented"

(**[add_piece pos piece square] adds [piece] to [pos] at [square]*)
let add_piece pos piece square =
  match square with rank, col -> pos.board.(rank).(col) <- piece

(**[letter chr] returns true if [chr] is a lowercase or uppercase
   letter, else false*)
let letter chr =
  match chr with 'a' .. 'z' -> true | 'A' .. 'Z' -> true | _ -> false

(**[fen_parse_castling str pos endex] takes in the second half of a fen
   string, a partially constructed position, and the index where the
   castling rights string ends and returns a boolean array of length 4
   storing the castling rights*)
let fen_parse_castling str pos endex =
  let boolArr = [| false; false; false; false |] in
  for i = 2 to endex do
    match str.[i] with
    | 'K' -> boolArr.(1) <- true
    | 'Q' -> boolArr.(0) <- true
    | 'q' -> boolArr.(2) <- true
    | 'k' -> boolArr.(3) <- true
    | _ -> ()
  done;
  boolArr

(**[fen_parse_other str pos] takes in a position with board array parsed
   from [str] and adds the en passant, castling, and turn information*)
let fen_parse_other str pos =
  let turn = if str.[0] = 'w' then true else false in
  let endingIndex = ref 0 in
  for i = 2 to 7 do
    if str.[i] = ' ' then endingIndex := i
  done;
  let castling = fen_parse_castling str pos !endingIndex in
  let passant_square = String.sub str !endingIndex 2 in
  let ep = sqr_from_str passant_square in
  {
    pos with
    turn;
    castling;
    ep;
    checked =
      attacked_square pos
        (if turn then pos.wking else pos.bking)
        (not turn);
  }

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
    | '1' -> fen_to_board_helper str pos (rank + 1) col nxt_ind
    | '2' -> fen_to_board_helper str pos (rank + 2) col nxt_ind
    | '3' -> fen_to_board_helper str pos (rank + 3) col nxt_ind
    | '4' -> fen_to_board_helper str pos (rank + 4) col nxt_ind
    | '5' -> fen_to_board_helper str pos (rank + 5) col nxt_ind
    | '6' -> fen_to_board_helper str pos (rank + 6) col nxt_ind
    | '7' -> fen_to_board_helper str pos (rank + 7) col nxt_ind
    | '8' -> fen_to_board_helper str pos rank col nxt_ind
    | _ ->
        raise
          (IllegalFen
             ( Char.escaped str.[ind]
             ^ " is not a valid FEN number or symbol" ))

and letter_fen_matching str pos rank col prevind nextind =
  let nk = ref (-1, -1) in
  let king_color = false in
  let piece =
    match str.[prevind] with
    | 'p' -> bpawn
    | 'r' -> brook
    | 'n' -> bknight
    | 'b' -> bbishop
    | 'q' -> bqueen
    | 'k' ->
        nk := (rank, col);
        blking
    | 'P' -> wpawn
    | 'R' -> wrook
    | 'N' -> wknight
    | 'B' -> wbishop
    | 'Q' -> wqueen
    | 'K' ->
        nk := (rank, col);
        whking
    | _ ->
        raise
          (IllegalFen
             (Char.escaped str.[prevind] ^ " is not a valid FEN letter"))
  in
  add_piece pos piece (rank, col);
  let new_pos =
    {
      pos with
      bking = (if king_color then pos.bking else !nk);
      wking = (if king_color then !nk else pos.wking);
    }
  in
  fen_to_board_helper str new_pos (rank + 1) col nextind

let fen_to_board str =
  let pos = init_empty () in
  fen_to_board_helper str pos 0 7 0

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

let equals pos1 pos2 = failwith "unimplemented"

let eval_move pos = failwith "unimplemented"
