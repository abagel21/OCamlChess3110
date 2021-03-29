open Piece
(**AF: the record {board, castling, ep, turn} represents a full chess position 
where board=[|[|a1...a8|];...;[|h1...h8|]|] represents the squares of the board, 
castling=[qw;kw;qb;kb] such that qw is a boolean representing whether white has 
queenside castling and the rest follow,
ep=(int, int) where the first int is the row and the second int is the column of 
the en passant square,
turn=bool where true is white's turn and false is black's turn
the string list of type r represents the move stack of the position,
prev=t represents the previous position state*)
(**RI=The t.board array is always full*)

type r = (Piece option) array array

type square = (int, int)

type s = (square, square) list

type t = {
  board : r;
  castling : bool array;
  ep : int tuple;
  turn: bool;
  move_stack : s;
  checked : bool;
  wking : square;
  bking : square;
}


let wrook = Some(make_piece true Rook)
let wknight = Some(make_piece true Knight)
let wbishop = Some(make_piece true Bishop)
let wking = Some(make_piece true King)
let wqueen = Some(make_piece true Queen)
let wpawn = Some(make_piece true Pawn)

let brook = Some(make_piece false Rook)
let bknight = Some(make_piece false Knight)
let bbishop = Some(make_piece false Bishop)
let bking = Some(make_piece false King)
let bqueen = Some(make_piece false Queen)
let bpawn = Some(make_piece false Pawn)


let init_board_array = () -> 
  [|[|wrook; wknight; wbishop; wqueen; wking; wbishop; wknight; wrook|];
  [|wpawn;wpawn;wpawn;wpawn;wpawn;wpawn;wpawn;wpawn|];
  [|None;None;None;None;None;None;None;None|];
  [|None;None;None;None;None;None;None;None|];
  [|None;None;None;None;None;None;None;None|];
  [|None;None;None;None;None;None;None;None|];
  [|bpawn;bpawn;bpawn;bpawn;bpawn;bpawn;bpawn;bpawn|];
  [|brook; bknight; bbishop; bking; bqueen; bbishop; bknight; brook|]]

let init : t = {
board=init_board_array;
castling=[true; true; true; true];
ep=(-1, -1);
turn=true;
prev=None;
checked = false;
wking = (0,4);
bking = (7,4);
}

let get_turn pos = pos.turn

let get_piece str pos = 
  let trm_str = String.trim str in
  if String.length trm_str = 2 then
    let rank = code (String.get str 0) - (code 'a') in
    let col = code (String.get str 1) - (code '1') in
    if rank > 7 || rank < 0 || col > 7 || rank < 0 
    then throw IllegalSquare(str ^ " is not a valid square")
    else pos.board(rank)(col)
  else throw IllegalSquare(str ^ " is not a legal square")


let rank_rep = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']


(**[verify_move_string str] checks whether [str] is a valid coordinate representation chess move. 'e2e4' is valid, 'p1p3' is not. Ranks must be in {a-h} and columns must be in {1-8}*)
let verify_move_string (str: string) = 
  let length = (String.length str) = 4 in
  let from_rank = String.get str 0 in
  let valid_fr = List.exists (fun x -> x = from_rank) rank_rep in
  let from_col = code (String.get str 1) - (code '0') in 
  let valid_fc = from_col > 0 && from_col <= 8 in
  let to_rank = String.get str 2 in
  let valid_tr = List.exists (fun x -> x = to_rank) rank_rep in
  let to_col = code (String.get str 3) - (code '0') in
  let valid_tc = to_col > 0 && to_col <= 8 in
  length && valid_fr && valid_fc && valid_tr && valid_tc

let get_piece_internal square pos =
  match square with 
  | (rank, col) -> pos.board(rank)(col)

let sqr_from_str str = 
  let rank = code (String.get str 0) - (code 'a') in
  let col = code (String.get str 1) - (code '1') in
  (rank, col)

let move str pos = 
  let trm_str = String.trim str in
  if verify_move_string trm_str then 
    let from_sqr = sqr_from_str (String.sub trm_str 0 2) in
    let to_sqr = sqr_from_str (String.sub trm_str 2 2) in
    if from_sqr = to_sqr 
      then throw IllegalMove("Cannot move piece to the square it is currently at")
    else
      let piece = get_piece_internal from_sqr pos in 
      match piece with
      | None -> throw IllegalMove(trm_str ^ " does not contain a valid from square")
      | Some (k) -> move_helper k pos from_sqr to_sqr
  else throw IllegalMove(trm_str ^ " is not a valid coordinate string of a move")

(**[move_rook pos from_sqr to_sqr] moves a rook from [from_sqr] to [to_sqr]*)
let move_normal_piece pos from_sqr to_sqr =
  match from_sqr, to_sqr with
  | (frank, fcol), (trank, tcol) ->  let curr_piece = pos.board(frank)(fcol) in
    pos.board(frank)(fcol) <- None;
    pos.board(trank, tcol) <- curr_piece;
    (from_sqr, to_sqr) :: pos.move_stack


let rook_check_helper pos from_sqr to_sqr =
  match from_sqr, to_sqr with
  | (frank, fcol), (trank, tcol) -> 
    if frank = trank then
      for i = fcol + 1 to tcol - 1 do
        if get_piece_internal (i, fcol) <> None 
          then throw IllegalMove("Pieces cannot move through other pieces") 
      done
      return true;
    else if fcol = tcol then
        for i = frank + 1 to trank - 1 do
          if get_piece_internal (frank, i) <> None 
            then throw IllegalMove("Pieces cannot move through other pieces")
        done
        return true;
    else throw IllegalMove("Rook must move horizontally or vertically")

(**[is_check pos] returns true if the player [get_turn pos] is in check, else false*)
let is_check pos =
  pos.checked

let find_king_sqr pos = 
  if get_turn pos then pos.wking else pos.bking

let will_be_checked pos from_sqr to_sqr = 
  let king = find_king_sqr pos in
  match king, from_sqr with
  | (krank, kcol), (frank, fcol) -> 
    if krank - frank = kcol - fcol then 
      (*check if attacked on diag*)
    else if 

  

let checked_move piece pos from_sqr to_sqr =
  match Piece.get_piece piece with 
  | Pawn -> 
  | Knight -> 
  | Bishop ->
  | Rook -> 
  | Queen ->
  | King ->


let move_helper piece pos from_sqr to_sqr= 
  if get_owner piece = get_turn pos then 
  if is_check pos then checked_move piece pos from_sqr to_sqr
  else 
  match Piece.get_piece piece with 
  | Pawn -> 
  | Knight -> 
  | Bishop
  | Rook -> if rook_check_helper pos from_sqr to_sqr then move_normal_piece pos from_sqr to_sqr
  | Queen
  | King ->
else throw IllegalMove((if get_turn pos then "White" else "Black") ^ " does not own this piece")


let undo_prev pos = pos.prev_state
(**TODO*)


(**
charles - king, bishop, queen
alex - pawn, knight, willbechecked *)