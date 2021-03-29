open Piece

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

type square = int * int

type t = {
  board : r;
  castling : bool array;
  ep : int * int;
  turn : bool;
  move_stack : (square * square) list;
  checked : bool;
  wking : square;
  bking : square;
}

let wrook = Some (make_piece true Rook)

let wknight = Some (make_piece true Knight)

let wbishop = Some (make_piece true Bishop)

let wking = Some (make_piece true King)

let wqueen = Some (make_piece true Queen)

let wpawn = Some (make_piece true Pawn)

let brook = Some (make_piece false Rook)

let bknight = Some (make_piece false Knight)

let bbishop = Some (make_piece false Bishop)

let bking = Some (make_piece false King)

let bqueen = Some (make_piece false Queen)

let bpawn = Some (make_piece false Pawn)

let init_board_array =
  [|
    [|
      wrook; wknight; wbishop; wqueen; wking; wbishop; wknight; wrook;
    |];
    [| wpawn; wpawn; wpawn; wpawn; wpawn; wpawn; wpawn; wpawn |];
    [| None; None; None; None; None; None; None; None |];
    [| None; None; None; None; None; None; None; None |];
    [| None; None; None; None; None; None; None; None |];
    [| None; None; None; None; None; None; None; None |];
    [| bpawn; bpawn; bpawn; bpawn; bpawn; bpawn; bpawn; bpawn |];
    [|
      brook; bknight; bbishop; bking; bqueen; bbishop; bknight; brook;
    |];
  |]

let init =
  {
    board = init_board_array;
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

let checked_move piece pos from_sqr to_sqr =
  match Piece.get_piece piece with
  | Pawn -> failwith "Unimplemented"
  | Knight -> failwith "Unimplemented"
  | Bishop -> failwith "Unimplemented"
  | Rook -> failwith "Unimplemented"
  | Queen -> failwith "Unimplemented"
  | King -> failwith "Unimplemented"

(**[move_rook pos from_sqr to_sqr] moves a rook from [from_sqr] to
   [to_sqr]*)
let move_normal_piece pos from_sqr to_sqr =
  match (from_sqr, to_sqr) with
  | (frank, fcol), (trank, tcol) ->
      let curr_piece = pos.board.(frank).(fcol) in
      pos.board.(frank).(fcol) <- None;
      pos.board.(trank).(tcol) <- curr_piece;
      {
        pos with
        move_stack = (from_sqr, to_sqr) :: pos.move_stack;
        turn = not pos.turn;
      }

(**[rook_check_helper pos from_sqr to_sqr] verifies that the moves
   (from_sqr, to_sqr) is a legal move for a rook*)
let rook_check_helper pos from_sqr to_sqr =
  let a = ref true in
  match (from_sqr, to_sqr) with
  | (frank, fcol), (trank, tcol) ->
      if frank = trank then
        for i = fcol + 1 to tcol - 1 do
          match get_piece_internal (frank, i) pos with
          | None -> a := !a && false
          | Some k -> a := !a && true
        done
      else if fcol = tcol then
        for i = frank + 1 to trank - 1 do
          match get_piece_internal (i, fcol) pos with
          | None -> a := !a && false
          | Some k -> a := !a && true
        done;
      !a

(**[bishop_check_helper pos from_sqr to_sqr] verifies that the moves
   (from_sqr, to_sqr) is a legal move for a bishop*)
let bishop_check_helper pos from_sqr to_sqr =
  let a = ref true in
  match (from_sqr, to_sqr) with
  | (frank, fcol), (trank, tcol) ->
      if trank - frank <> tcol - fcol then false
      else
        let b = trank - frank in
        if frank < trank then
          for i = 1 to b - 1 do
            match get_piece_internal (frank + i, fcol + 1) pos with
            | None -> a := !a && false
            | Some k -> a := !a && true
          done
        else
          for i = b + 1 to -1 do
            match get_piece_internal (frank + i, fcol + i) pos with
            | None -> a := !a && false
            | Some k -> a := !a && true
          done;
        !a

(**[queen_check_helper pos from_sqr to_sqr] verifies that the moves
   (from_sqr, to_sqr) is a legal move for a queen*)
let queen_check_helper pos from_sqr to_sqr =
  rook_check_helper pos from_sqr to_sqr
  || bishop_check_helper pos from_sqr to_sqr

(**[king_check_helper pos from_sqr to_sqr] verifies that the moves
   (from_sqr, to_sqr) is a legal move for a king*)
let king_check_helper pos from_sqr to_sqr = failwith ""

(**[is_check pos] returns true if the player [get_turn pos] is in check,
   else false*)
let is_check pos = pos.checked

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
let bishop_checks pos square = failwith "unimplemented"

(**[rook_checks pos square] returns true if the rook on [square] checks
   the opposing player's king*)
let rook_checks pos square = failwith "unimplemented"

(**[queen_checks pos square] returns true if the queen on [square]
   checks the opposing player's king*)
let queen_checks pos square = failwith "unimplemented"

(**[piece_causes_check pos square] checks if the piece on [square]
   causes check for the opposing king*)
let piece_causes_check pos square =
  let piece = get_piece_internal square pos in
  match piece with
  | None -> false
  | Some k -> (
      match Piece.get_piece k with
      | Pawn -> failwith ""
      | Knight -> failwith ""
      | Bishop -> failwith ""
      | Rook -> failwith ""
      | Queen -> failwith ""
      | King -> failwith "" )

let possibly_castle pos from_square to_square = failwith "unimplemented"

let will_be_checked pos from_sqr to_sqr =
  let king = find_king_sqr pos (get_turn pos) in
  match (king, from_sqr) with
  | (krank, kcol), (frank, fcol) ->
      if krank - frank = kcol - fcol then failwith "unimplemented"
      else if true then failwith ""
      else failwith ""

let checked_move piece pos from_sqr to_sqr : t =
  match Piece.get_piece piece with
  | Pawn -> failwith ""
  | Knight -> failwith ""
  | Bishop -> failwith ""
  | Rook -> failwith ""
  | Queen -> failwith ""
  | King -> failwith ""

let move_helper piece pos from_sqr to_sqr =
  if get_owner piece = get_turn pos then
    if is_check pos then checked_move piece pos from_sqr to_sqr
    else
      match Piece.get_piece piece with
      | Pawn -> pos
      | Knight -> pos
      | Bishop ->
          if bishop_check_helper pos from_sqr to_sqr then
            move_normal_piece pos from_sqr to_sqr
          else pos
      | Rook ->
          if rook_check_helper pos from_sqr to_sqr then
            move_normal_piece pos from_sqr to_sqr
          else pos
      | Queen ->
          if queen_check_helper pos from_sqr to_sqr then
            move_normal_piece pos from_sqr to_sqr
          else pos
      | King ->
          if king_check_helper pos from_sqr to_sqr then
            possibly_castle pos from_sqr to_sqr
          else pos
  else
    raise
      (IllegalMove
         ( (if get_turn pos then "White" else "Black")
         ^ " does not own this piece" ))

let move str pos =
  let trm_str = String.trim str in
  if verify_move_string trm_str then
    let from_sqr = sqr_from_str (String.sub trm_str 0 2) in
    let to_sqr = sqr_from_str (String.sub trm_str 2 2) in
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
      | Some k -> move_helper k pos from_sqr to_sqr
  else
    raise
      (IllegalMove
         (trm_str ^ " is not a valid coordinate string of a move"))

let undo_prev pos = failwith "Unimplemented"

(** charles - king, bishop, queen alex - pawn, knight, willbechecked *)
