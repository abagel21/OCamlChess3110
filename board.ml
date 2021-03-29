open Piece

(**TODO 1. Implement pawn movement (including en passant) 2. implement
   castling 3. piece_causes_check 4. implement pins (if a move causes
   the player to be in check) 5. Implement checkmate/draw checking*)

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

exception IllegalFen

exception IllegalPiece

exception EmptyMoveStack

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

(**[init_empty_board_array] returns board array that is completely empty *)
let init_empty_board_array =
  [|
    [| None; None; None; None; None; None; None; None |];
    [| None; None; None; None; None; None; None; None |];
    [| None; None; None; None; None; None; None; None |];
    [| None; None; None; None; None; None; None; None |];
    [| None; None; None; None; None; None; None; None |];
    [| None; None; None; None; None; None; None; None |];
    [| None; None; None; None; None; None; None; None |];
    [| None; None; None; None; None; None; None; None |];
  |]

(**[init_empty] returns a board representation that is empty *)
let init_empty =
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

(**[rook_valid_helper pos from_sqr to_sqr] verifies that the move
   (from_sqr, to_sqr) is a legal move for a rook. *)
let rook_valid_helper pos from_sqr to_sqr =
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

(**[knight_valid_helper pos from_sqr to_sqr] ensures that the move
   (from_sqr, to_sqr) is legal for a knight and if so, executes the move
   and returns the new state. Throws: IllegalMove if the move is not
   legal for a knight *)
let knight_valid_helper pos from_sqr to_sqr =
  match (from_sqr, to_sqr) with
  | (frank, fcol), (trank, tcol) ->
      if abs (frank - trank) = 1 && abs (fcol - tcol) = 2 then true
      else if abs (frank - trank) = 2 && abs (fcol - tcol) = 1 then true
      else raise (IllegalMove "Illegal move for knight")

(**[queen_check_helper pos from_sqr to_sqr] verifies that the moves
   (from_sqr, to_sqr) is a legal move for a queen*)
let queen_valid_helper pos from_sqr to_sqr =
  rook_valid_helper pos from_sqr to_sqr
  || bishop_valid_helper pos from_sqr to_sqr

(**[king_valid_helper pos from_sqr to_sqr] verifies that the moves
   (from_sqr, to_sqr) is a legal move for a king*)
let king_valid_helper pos from_sqr to_sqr = failwith ""

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
        checked = piece_causes_check pos to_sqr;
        ep = (-1, -1);
      }

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

let promote square pos piece =
  match square with
  | rank, col ->
      pos.board.(rank).(col) <- piece;
      pos

(**[pawn_dbmv_chk pos from_sqr to_sqr trank fcol] takes in the current
   position, move from_sqr and to_sqr, and returns true if the square in
   front of [from_sqr] and [to_sqr] are unoccupied, else throws
   IllegalMove*)
let pawn_dbmv_chk pos from_sqr to_sqr trank fcol =
  match (from_sqr, to_sqr) with
  | (frank, fcol), (trank, tcol) ->
      let afree =
        match get_piece_internal to_sqr pos with
        | None -> true
        | Some k -> false
      in
      let between_rank =
        if get_turn pos then trank - 1 else trank + 1
      in
      let bfree =
        match get_piece_internal (between_rank, fcol) pos with
        | None -> true
        | Some k -> false
      in
      afree && bfree

(**[pawn_double_move_helper pos from_sqr to_sqr] checks that the square
   in front of and two squares in front of the pawn at [from_sqr] are
   empty and executes the move if so, else throws IllegalMove. Requires:
   from_sqr is a pawn*)
let pawn_double_move_helper pos from_sqr to_sqr =
  match (from_sqr, to_sqr) with
  | (frank, fcol), (trank, tcol) ->
      if
        (get_turn pos && frank = 1)
        || (get_turn pos = false && frank = 6)
      then
        if pawn_dbmv_chk pos from_sqr to_sqr trank fcol then
          move_normal_piece pos from_sqr to_sqr
        else raise (IllegalMove "Illegal move for a pawn")
      else raise (IllegalMove "Illegal move for a pawn")

(**[pawn_valid_helper pos from_sqr to_sqr] verifies that the move
   (from_sqr, to_sqr) is a legal move for a pawn and executes it,
   returning the new state. Throws: IllegalMove if the move is illegal
   Requires: from_sqr is a pawn*)
let pawn_valid_helper pos from_sqr to_sqr new_p =
  match (from_sqr, to_sqr) with
  | (frank, fcol), (trank, tcol) ->
      let next_pos =
        if
          (get_turn pos && trank - frank = 1)
          || (get_turn pos && trank - frank = -1)
        then
          if fcol <> tcol && to_sqr = pos.ep then
            match get_piece_internal to_sqr pos with
            | None -> move_en_passant pos from_sqr to_sqr
            | Some k -> move_normal_piece pos from_sqr to_sqr
          else if fcol = tcol then move_normal_piece pos from_sqr to_sqr
          else raise (IllegalMove "Illegal move for a pawn")
        else if
          (get_turn pos && trank - frank = 2)
          || (get_turn pos && trank - frank = -2)
        then pawn_double_move_helper pos from_sqr to_sqr
        else raise (IllegalMove "Illegal move for a pawn")
      in
      promote to_sqr next_pos new_p

(**[is_check pos] returns true if the player [get_turn pos] is in check,
   else false*)
let is_check pos = pos.checked

let possibly_castle pos from_square to_square = failwith "unimplemented"

(**[will_be_checked pos from_sqr to_sqr] checks whether *)
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
  if get_owner piece = get_turn pos then
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

let letter chr =
  match chr with 'a' .. 'z' -> true | 'A' .. 'Z' -> true | _ -> false

let rec fen_to_board_helper str pos rank col ind =
  let next_rank = if col = 7 then rank - 1 else rank in
  let next_col = if col = 7 then 0 else col + 1 in
  let nxt_ind = ind + 1 in
  if rank < 0 then pos
  else if letter str.[ind] then
    letter_fen_matching str pos next_rank next_col nxt_ind
  else
    match str.[ind] with
    | '/' -> fen_to_board_helper str pos (rank - 1) 0 nxt_ind
    | '1' -> fen_to_board_helper str pos rank (col + 1) nxt_ind
    | '2' -> fen_to_board_helper str pos rank (col + 2) nxt_ind
    | '3' -> fen_to_board_helper str pos rank (col + 3) nxt_ind
    | '4' -> fen_to_board_helper str pos rank (col + 4) nxt_ind
    | '5' -> fen_to_board_helper str pos rank (col + 5) nxt_ind
    | '6' -> fen_to_board_helper str pos rank (col + 6) nxt_ind
    | '7' -> fen_to_board_helper str pos rank (col + 7) nxt_ind
    | '8' -> fen_to_board_helper str pos (rank - 1) 0 nxt_ind
    | _ -> raise IllegalFen

and letter_fen_matching str pos rank col ind =
  let piece =
    match str.[ind] with
    | 'p' -> bpawn
    | 'r' -> brook
    | 'n' -> bknight
    | 'b' -> bbishop
    | 'q' -> bqueen
    | 'k' -> bking
    | 'P' -> wpawn
    | 'R' -> wrook
    | 'N' -> wknight
    | 'B' -> wbishop
    | 'Q' -> wqueen
    | 'K' -> wking
    | _ -> raise IllegalFen
  in
  add_piece pos piece (rank, col);
  fen_to_board_helper str pos rank col ind

let fen_to_board str =
  let pos = init_empty in
  fen_to_board_helper str pos 7 0 0

let to_string pos =
  let rec to_string_helper pos rank col =
    let color = get_turn pos in
    let next_rank = if col = 7 then rank - 1 else rank in
    let next_col = if col = 7 then 0 else col + 1 in
    match get_piece_internal (rank, col) pos with
    | None -> " |  " ^ to_string_helper pos next_rank next_col
    | Some k -> (
        match Piece.get_piece k with
        | Pawn ->
            " | "
            ^ (if color then "P" else "p")
            ^ to_string_helper pos next_rank next_col
        | Knight ->
            " | "
            ^ (if color then "N" else "n")
            ^ to_string_helper pos next_rank next_col
        | Bishop ->
            " | "
            ^ (if color then "B" else "b")
            ^ to_string_helper pos next_rank next_col
        | Rook ->
            " | "
            ^ (if color then "R" else "r")
            ^ to_string_helper pos next_rank next_col
        | Queen ->
            " | "
            ^ (if color then "Q" else "q")
            ^ to_string_helper pos next_rank next_col
        | King ->
            " | "
            ^ (if color then "K" else "k")
            ^ to_string_helper pos next_rank next_col )
  in
  to_string_helper pos 7 0

let equals pos1 pos2 = failwith "unimplemented"

let eval_move pos = failwith "unimplemented"

(** charles - king, bishop, queen alex - pawn, knight, willbechecked *)
