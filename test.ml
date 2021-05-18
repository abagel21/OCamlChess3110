(**Testing Philosophy and Correctness Although we do test helper
   functions such as [get_piece] and [load_fen] in order to ensure the
   correctness of all exposed API endpoints, by the nature of our
   program a significant part of our testing is localized upon one
   function: [move]. For this function, our specification is more
   significantly defined by the rules of chess than anything we can
   explicitly write, thus black box testing, although utilized to some
   extent, is extremely challenging. To a significant extent we focused
   on separating our code in board.ml into functions that deal with
   different pieces of the rules of chess, such as pawns moving twice,
   en passant, knight capturing, castling, etc and then used glass box
   testing with bisect to ensure we were covering all corner cases.
   However, again, because the rules of chess are so large, we
   supplemented this coverage with our understanding of the game of
   chess to ensure that all legal moves are covered and all illegal
   moves are blocked. These two eventualities fall mostly into the two
   testing functions [move_throws] and [move_no_throw] where we feed the
   move and the expected outcome, then through each possibility for a
   move--a knight move, a king move, check, en passant, castling--we
   write a series of tests to ensure that legal moves result in the
   correct final board and illegal moves throw the correct informative
   error. We also tested the move generator and undo_move with
   randomized testing by playing a random game with the move generator a
   large number of times with a large number of moves to ensure no
   exceptions are thrown and, in the case of undo_move, the previous
   board state is maintained. This set of testing covers all of the
   board.ml API other than draw_board, which relates to the GUI. Through
   our own development of the rules of chess in separate functions in
   our code and then sufficient glass box coverage of this code and
   supplementary black box tests from our understanding of the rules of
   chess, with a tiny bit of random testing, we ensured that our
   interface was correct. As far as the actual game, our testing was
   manual. Running make play with all the different gamemodes in main.ml
   allowed us to test that the game loops cycled correctly and
   interacted with the API of board.ml correctly by feeding outcomes
   from calling the functions and informative exceptions to the user.
   Furthermore, we also tested the engine manually. Because it is
   imperfect, there is no way to assure it makes the "correct" move,
   only that it makes reasonable ones with the information given.
   Through this, we ensured that the underlying functionality of our
   chess game in board.ml, and then that the method in which the user
   interacts with this functionality is also correct. As we write this,
   our bisect coverage of board.ml is 98% and we have done countless
   runs through the software, giving us confidence that this strategy
   has been implemented to its fullest.*)

open OUnit2
open Board
open Piece

let board = init ()

let get_piece_tester name board square expected =
  name >:: fun _ -> assert_equal expected (Board.get_piece square board)

let get_piece_throws name board square =
  name >:: fun _ ->
  assert_raises
    (IllegalSquare (square ^ " is not a valid square"))
    (fun () -> Board.get_piece square board)

let get_piece_tests =
  [
    get_piece_tester "getting rook on starting position works" board
      "a1" "R";
    get_piece_tester "getting empty square works" board "e4" "NA";
    get_piece_throws "out of bounds square greater throws" board "u7";
    get_piece_throws "out of bounds number greater throws" board "a9";
    get_piece_throws "out of bounds number less than throws" board "a+";
    get_piece_throws "out of bounds square less than throws" board "^5";
    get_piece_throws "longer string throws" board "pawn";
  ]

let string_of_pieces pieces =
  match pieces with
  | { pawns; knights; bishops; rooks; queens } ->
      "{pawns=" ^ string_of_int pawns ^ "; knights="
      ^ string_of_int knights ^ "; bishops=" ^ string_of_int bishops
      ^ "; rooks=" ^ string_of_int rooks ^ "; queens="
      ^ string_of_int queens ^ "}"

let pieces_printer pieces =
  let wpieces, bpieces = pieces in
  "(" ^ string_of_pieces wpieces ^ ", " ^ string_of_pieces bpieces ^ ")"

let gp_fen_tester name expected fen =
  let board = load_fen fen in
  name >:: fun _ ->
  assert_equal expected (Board.get_pieces board) ~printer:pieces_printer

let gp_ep_tester name fen move_str =
  let board = load_fen fen in
  let wpieces, bpieces = Board.get_pieces board in
  let next_board = move move_str "" board in
  let expected =
    ( ( if Board.get_turn board then wpieces
      else { wpieces with pawns = wpieces.pawns - 1 } ),
      if Board.get_turn board then
        { bpieces with pawns = bpieces.pawns - 1 }
      else bpieces )
  in
  name >:: fun _ ->
  assert_equal expected
    (Board.get_pieces next_board)
    ~printer:pieces_printer

let gp_move_tester name fen move_str expected =
  let board = load_fen fen in
  let next_board = move move_str "Q" board in
  name >:: fun _ ->
  assert_equal expected
    (Board.get_pieces next_board)
    ~printer:pieces_printer

let init_fen =
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let ep_fen =
  "2bqk1nr/ppp1p1pp/8/2npPp2/1r1P3b/2P3P1/PP3P1P/RNBQKBNR w KQk f6 0 1"

let promote_fen = "rnbqkbnr/7p/8/8/4Pp2/8/pppp1PpP/4B1KR b Kkq e3 0 1"

let init_pieces =
  ( { pawns = 8; knights = 2; bishops = 2; rooks = 2; queens = 1 },
    { pawns = 8; knights = 2; bishops = 2; rooks = 2; queens = 1 } )

let n_capture_pieces =
  ( { pawns = 8; knights = 2; bishops = 2; rooks = 2; queens = 1 },
    { pawns = 8; knights = 1; bishops = 2; rooks = 2; queens = 1 } )

let r_capture_pieces =
  ( { pawns = 8; knights = 2; bishops = 2; rooks = 2; queens = 1 },
    { pawns = 8; knights = 2; bishops = 2; rooks = 1; queens = 1 } )

let b_capture_pieces =
  ( { pawns = 8; knights = 2; bishops = 2; rooks = 2; queens = 1 },
    { pawns = 8; knights = 2; bishops = 1; rooks = 2; queens = 1 } )

let promote_pieces =
  ( { pawns = 3; knights = 0; bishops = 1; rooks = 1; queens = 0 },
    { pawns = 6; knights = 2; bishops = 2; rooks = 2; queens = 2 } )

let get_pieces_tests =
  [
    gp_fen_tester "initial board has correct pieces" init_pieces
      init_fen;
    gp_move_tester "initial board with e2e4 still has same pieces"
      init_fen "e2e4" init_pieces;
    gp_ep_tester "en passant removes a pawn correctly" ep_fen "e5f6";
    gp_move_tester "capturing black knight removes black knight" ep_fen
      "d4c5" n_capture_pieces;
    gp_move_tester "capturing a black rook removes a black rook" ep_fen
      "c3b4" r_capture_pieces;
    gp_move_tester "capturing a black bishop removes a black bishop"
      ep_fen "g3h4" b_capture_pieces;
    gp_move_tester "promoting to queen adds a black queen" promote_fen
      "b2b1" promote_pieces;
  ]

let to_string_tester name fen expected =
  let board = load_fen fen in
  name >:: fun _ ->
  assert_equal expected (Board.to_string board) ~printer:(fun x -> x)

let s_fen = "r1b2rk1/1pp1qpp1/7p/3Qp3/N7/3P3P/2P1NPP1/R4RK1 w Qq - 0 1"

let s_fen_string =
  " | r |   | b |   |   | r | k |   | 8\n\
  \ |   | p | p |   | q | p | p |   | 7\n\
  \ |   |   |   |   |   |   |   | p | 6\n\
  \ |   |   |   | Q | p |   |   |   | 5\n\
  \ | N |   |   |   |   |   |   |   | 4\n\
  \ |   |   |   | P |   |   |   | P | 3\n\
  \ |   |   | P |   | N | P | P |   | 2\n\
  \ | R |   |   |   |   | R | K |   | 1\n\
  \   a   b   c   d   e   f   g   h"

let init_string =
  " | r | n | b | q | k | b | n | r | 8\n\
  \ | p | p | p | p | p | p | p | p | 7\n\
  \ |   |   |   |   |   |   |   |   | 6\n\
  \ |   |   |   |   |   |   |   |   | 5\n\
  \ |   |   |   |   |   |   |   |   | 4\n\
  \ |   |   |   |   |   |   |   |   | 3\n\
  \ | P | P | P | P | P | P | P | P | 2\n\
  \ | R | N | B | Q | K | B | N | R | 1\n\
  \   a   b   c   d   e   f   g   h"

let to_string_tests =
  [
    to_string_tester "complex board position produces correct string"
      s_fen s_fen_string;
    to_string_tester "initial position translates to string correctly"
      init_fen init_string;
  ]

let get_moves_tester name moves =
  let board = move_list moves (init ()) in
  name >:: fun _ -> assert_equal moves (get_moves board)

let get_moves_tests =
  [
    get_moves_tester "empty move list is empty" [];
    get_moves_tester "one move gives correct list back" [ ("e2e4", "") ];
    get_moves_tester
      "long list of captures gives correct move list back"
      [
        ("e2e4", "");
        ("e7e5", "");
        ("g1f3", "");
        ("b8c6", "");
        ("f1c4", "");
        ("g8f6", "");
        ("b1c3", "");
        ("f8d6", "");
        ("d1e2", "");
        ("h8f8", "");
        ("d2d3", "");
        ("a7a5", "");
        ("f3e5", "");
        ("d6e5", "");
        ("c3d5", "");
        ("f6d5", "");
        ("c4d5", "");
        ("d8h4", "");
        ("d5c6", "");
        ("b7c6", "");
        ("c1e3", "");
        ("e5h2", "");
        ("h1h2", "");
        ("h4h2", "");
        ("e2g4", "");
        ("h2h1", "");
        ("e1d2", "");
        ("h1a1", "");
        ("g4g7", "");
        ("a1a2", "");
      ];
  ]

let make_col board colid =
  let row = ref "" in
  for i = 1 to 8 do
    row := !row ^ Board.get_piece (colid ^ string_of_int i) board
  done;
  !row

let init_pos_test name board colid expected =
  let col = make_col board colid in
  name >:: fun _ -> assert_equal col expected

let init_tests =
  [
    init_pos_test "First col is RPNANANANApr" board "a" "RPNANANANApr";
    init_pos_test "Second col is NPNANANANApn" board "b" "NPNANANANApn";
    init_pos_test "Third col is BPNANANANApb" board "c" "BPNANANANApb";
    init_pos_test "Fourth col is QPNANANANApq" board "d" "QPNANANANApq";
    init_pos_test "Fifth col is KPNANANANApk" board "e" "KPNANANANApk";
    (*Cols 6-8 are repeats of 1-3*)
  ]

(**[check_true name fen move_str] creates an OUnit test asserting that
   the following player is in check after the move indicated by
   [move_str]*)
let check_true name fen move_str promote_str =
  let pos = load_fen fen in
  let next_pos = move move_str promote_str pos in
  name >:: fun _ -> assert_equal true (is_in_check next_pos)

(**[check_false name fen move_str] creates an OUnit test asserting that
   performing [move_str] on the position created from [fen] does not
   result in the following player being in check *)
let check_false name fen move_str promote_str =
  let pos = load_fen fen in
  let next_pos = move move_str promote_str pos in
  name >:: fun _ -> assert_equal false (is_in_check next_pos)

(**[move_throws board from_sqr to_sqr expected] creates an OUnit test
   asserting that movement of a piece at [from_sqr] to [to_sqr] throws
   [IllegalMove of error]*)
let move_throws name board move_str error =
  let pos = load_fen board in
  name >:: fun _ ->
  assert_raises (IllegalMove error) (fun () -> move move_str "" pos)

(**[move_no_throw] asserts that move completes without throwing an error
   and the correct piece is at final_pos*)
let move_no_throw name board move_str final_pos expected =
  let pos = load_fen board in
  name >:: fun _ ->
  assert_equal
    (Board.get_piece final_pos (move move_str "" pos))
    expected
    ~printer:(fun x -> x)

let rec all_empty board empty_lst =
  match empty_lst with
  | [] -> true
  | h :: t -> Board.get_piece h board = "NA" && all_empty board t

(**[move_seq_nt] returns an OUnit test asserting that executing the
   three move sequence [move_tpl] results in a final position with
   [expected] at [final_pos] and empty squares at all strings in
   [empty_lst] *)
let move_seq_nt name board move_tpl final_pos empty_lst expected =
  let pos = load_fen board in
  match move_tpl with
  | mv1, mv2, mv3 ->
      let next_board =
        pos |> move mv1 "" |> move mv2 "" |> move mv3 ""
      in
      let pawn_posc = Board.get_piece final_pos next_board = expected in
      let pawn_rmvd = all_empty next_board empty_lst in
      name >:: fun _ ->
      assert_bool
        "either pawn was not in correct final position or en passant \
         pawn was not removed"
        (pawn_posc && pawn_rmvd)

(**[en_passant_nt] returns an OUnit test asserting that executing move
   with [move_str] on [board] results in a position with [expected] at
   [final_pos] and with [empty_sqr] having no piece on it *)
let en_passant_nt name board move_str final_pos empty_sqr expected =
  let pos = load_fen board in
  let next_board = move move_str "" pos in
  let pawn_posc = Board.get_piece final_pos next_board = expected in
  let pawn_rmvd = Board.get_piece empty_sqr next_board = "NA" in
  name >:: fun _ ->
  assert_bool
    "either pawn was not in correct final position or en passant pawn \
     was not removed"
    (pawn_posc && pawn_rmvd)

(*FENs for knight tests*)
let nf1 = "8/8/4p3/8/3N4/1n3q2/4b3/8 w - - 0 1"

let nf2 = "8/8/5r1p/4q3/6n1/4k3/5b1n/8 b - - 0 1"

let capture_knight = "7K/8/8/2pppN2/2pkp3/2ppp3/8/8 w - - 0 1"

let knight_tests =
  [
    move_no_throw "white knight moves midtop right normally" nf1 "d4f5"
      "f5" "N";
    move_no_throw "white knight captures top right pawn normally" nf1
      "d4e6" "e6" "N";
    move_no_throw "white knight moves top left normally" nf1 "d4c6" "c6"
      "N";
    move_no_throw "white knight moves midtop left knight normally" nf1
      "d4b5" "b5" "N";
    move_no_throw "white knight captures midbottom left normally" nf1
      "d4b3" "b3" "N";
    move_no_throw "white knight moves bottom left normally" nf1 "d4c2"
      "c2" "N";
    move_no_throw "white knight captures bottom right bishop normally"
      nf1 "d4e2" "e2" "N";
    move_no_throw "white knight captures midbottom right queen normally"
      nf1 "d4f3" "f3" "N";
    move_throws "black knight capturing black pawn throws" nf2 "g4h6"
      "Cannot capture ally";
    move_throws "black knight capturing black rook throws" nf2 "g4f6"
      "Cannot capture ally";
    move_throws "black knight capturing black queen throws" nf2 "g4e5"
      "Cannot capture ally";
    move_throws "black knight capturing black king throws" nf2 "g4e3"
      "Cannot capture ally";
    move_throws "black knight capturing black bishop throws" nf2 "g4f2"
      "Cannot capture ally";
    move_throws "black knight capturing black knight throws" nf2 "g4h2"
      "Cannot capture ally";
    move_throws "black knight moving out of bounds throws" nf2 "g4i3"
      "g4i3 is not a valid coordinate string of a move";
    move_throws "black knight moving abnormally horizontally throws" nf2
      "g4e4" "Illegal move for a knight";
    move_throws "black knight moving abnormally diagonally throws" nf2
      "g4e6" "Illegal move for a knight";
    move_throws
      "black knight moving abnormally one square diagonally throws" nf2
      "g4h3" "Illegal move for a knight";
    move_throws "cannot capture king with knight" capture_knight "f5d4"
      "Cannot capture king";
  ]

(*FENs for bishop tests*)
let bf1 = "6k1/4n3/8/8/1B6/8/3r4/1K6 w - - 0 1"

let bishop_tests =
  [
    move_no_throw "bishop moves top right normally" bf1 "b4d6" "d6" "B";
    move_no_throw "bishop moves top left normally" bf1 "b4a5" "a5" "B";
    move_no_throw "bishop moves bottom left normally" bf1 "b4a3" "a3"
      "B";
    move_no_throw "bishop moves bottom right normally" bf1 "b4c3" "c3"
      "B";
    move_no_throw "bishop captures normally" bf1 "b4e7" "e7" "B";
    move_no_throw "bishop captures bottom right normally" bf1 "b4d2"
      "d2" "B";
    move_throws "bishop cannot move past piece" bf1 "b4f8"
      "Illegal move for a bishop";
    move_throws "bishop cannot move vertically" bf1 "b4b7"
      "Illegal move for a bishop";
    move_throws "bishop cannot move horizontally" bf1 "b4d4"
      "Illegal move for a bishop";
  ]

(*FENs for rook tests*)
let rf1 = "8/3Q4/8/8/3R2p1/8/3P4/8 w - - 0 1"

let rf2 = "rnbqkbnr/1pppppp1/8/p6p/P6P/8/1PPPPPP1/RNBQKBNR w KQkq - 0 1"

let rf3 = "rnbqkbnr/1pppppp1/8/p6p/P6P/8/1PPPPPP1/RNBQKBNR b KQkq - 0 1"

let rook_sets_castling name board move_str expected =
  let castling = get_castling (move move_str "" (load_fen board)) in
  name >:: fun _ -> assert_equal expected castling

let rook_tests =
  [
    move_no_throw "move white rook 1 square right works" rf1 "d4e4" "e4"
      "R";
    move_no_throw "move white rook 1 square left works" rf1 "d4c4" "c4"
      "R";
    move_no_throw "move white rook 1 square up works" rf1 "d4d5" "d5"
      "R";
    move_no_throw "move white rook 1 square down works" rf1 "d4d3" "d3"
      "R";
    move_no_throw "white rook captures black pawn works" rf1 "d4g4" "g4"
      "R";
    move_throws "white rook moves past black piece" rf1 "d4h4"
      "Illegal move for a rook";
    move_throws "white rook tries to capture white pawn" rf1 "d4d2"
      "Cannot capture ally";
    move_throws "white rook tries to move past white piece" rf1 "d4d1"
      "Illegal move for a rook";
    move_throws "white rook tries to capture white queen" rf1 "d4d7"
      "Cannot capture ally";
    move_throws "white rook tries to move past white queen" rf1 "d4d8"
      "Illegal move for a rook";
    rook_sets_castling
      "moving white kingside rook sets kingside castling to false" rf2
      "h1h2"
      [ false; true; true; true ];
    rook_sets_castling
      "moving white queenside rook sets queenside castling to false" rf2
      "a1a2"
      [ true; false; true; true ];
    rook_sets_castling
      "moving black kingside rook sets kingside castling to false" rf3
      "h8h7"
      [ true; true; false; true ];
    rook_sets_castling
      "moving black queenside rook sets queenside castling to false" rf3
      "a8a7"
      [ true; true; true; false ];
  ]

(*FENs for queen tests*)
let qf1 = "8/3q4/8/8/3q2P1/8/3p4/8 b - - 0 1"

let queen_tests =
  [
    move_no_throw "move black queen 1 square right works" qf1 "d4e4"
      "e4" "q";
    move_no_throw "move black queen 1 square left works" qf1 "d4c4" "c4"
      "q";
    move_no_throw "move black queen 1 square up works" qf1 "d4d5" "d5"
      "q";
    move_no_throw "move black queen 1 square down works" qf1 "d4d3" "d3"
      "q";
    move_no_throw "black queen captures white pawn works" qf1 "d4g4"
      "g4" "q";
    move_throws "black queen moves past black piece" qf1 "d4h4"
      "Illegal move for a queen";
    move_throws "black queen tries to capture black pawn" qf1 "d4d2"
      "Cannot capture ally";
    move_throws "black queen tries to move past black piece" qf1 "d4d1"
      "Illegal move for a queen";
    move_throws "black queen tries to capture black queen" qf1 "d4d7"
      "Cannot capture ally";
    move_throws "black queen tries to move past black queen" qf1 "d4d8"
      "Illegal move for a queen";
    move_throws "black cannot move a white pawn" qf1 "g4g5"
      "Black does not own the piece on g4";
  ]

let promote_tester name board move_str promote_str =
  let pos = load_fen board in
  let board = move move_str promote_str pos in
  name >:: fun _ ->
  assert_equal promote_str
    (String.uppercase_ascii
       (Board.get_piece (String.sub move_str 2 2) board))
    ~printer:(fun x -> x)

let promote_throws name board move_str promote_str error =
  let pos = load_fen board in
  name >:: fun _ ->
  assert_raises (IllegalPiece error) (fun x ->
      move move_str promote_str pos)

(*FENs for pawn tests*)
let pf1 = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let pf2 = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"

let pf3 =
  "2bqk1nr/ppp1p1pp/8/2npPp2/1r1P3b/2P3P1/PP3P1P/RNBQKBNR w KQk f6 0 1"

let pf4 = "rnbqkbnr/4p2p/8/8/4Pp2/8/pppp1PpP/4B1KR b Kkq e3 0 1"

let pf5 = "8/8/8/8/2b5/8/2P5/2K5 w - - 0 1"

let pf6 =
  "2bqk1nr/ppp1pppp/8/2np4/1r1PP2b/2P3P1/PP3P1P/RNBQKBNR w KQk - 0 1"

let pf6_seq = ("e4e5", "f7f5", "e5f6")

let pf7 =
  "rnbqkbnr/pppp1ppp/B7/4p3/4P3/8/PPPP1PPP/RNBQK1NR b KQkq - 0 1"

let pf8 = "8/1k1q2P1/8/8/3q4/8/3p4/3K4 w - - 0 1"

let promote_fen_alt =
  "rnbqkbnr/7p/8/8/4Pp2/1K6/pppp1PpP/4B2R b kq e3 0 1"

let promote_fen_alt2 =
  "rnbqkbnr/7p/8/8/4Pp2/8/ppp2PpP/4K2R b Kkq e3 0 1"

let promote_fen_alt3 = "8/2PPPPP1/4k3/8/8/8/8/1K6 w - - 0 1"

let promote_fen_alt4 = "8/2PPkPP1/8/8/8/8/8/1K6 w - - 0 1"

let promote_fen_alt5 = "rnbqkbnr/7p/8/8/4P3/8/ppKpp1pP/7R b kq e3 0 1"

let pawn_tests =
  [
    move_no_throw "white double pawn move at start does not throw" pf1
      "e2e4" "e4" "P";
    move_no_throw "white single pawn move at start does not throw" pf1
      "e2e3" "e3" "P";
    move_no_throw
      "white double pawn move from different position at start does \
       not throw"
      pf1 "f2f4" "f4" "P";
    move_no_throw
      "white single pawn move from different position at start does \
       not throw"
      pf1 "a2a3" "a3" "P";
    move_no_throw "black double pawn move at start does not throw" pf2
      "e7e5" "e5" "p";
    move_no_throw "black single pawn move at start does not throw" pf2
      "e7e6" "e6" "p";
    move_no_throw
      "black double pawn move from different position at start does \
       not throw"
      pf2 "g7g6" "g6" "p";
    move_no_throw
      "black single pawn move from different position at start does \
       not throw"
      pf2 "b7b5" "b5" "p";
    move_seq_nt
      "the moves that create an en passant square and the en passant \
       move themselves function correctly"
      pf6 pf6_seq "f6" [ "e4"; "e5"; "f5" ] "P";
    move_no_throw "capturing knight left correctly does not throw" pf3
      "d4c5" "c5" "P";
    move_no_throw "capturing rook left correctly does not throw" pf3
      "c3b4" "b4" "P";
    move_no_throw "capturing bishop right correctly does not throw" pf3
      "g3h4" "h4" "P";
    move_throws "incorrect en passant throws" pf3 "e5d6"
      "Illegal move for a pawn";
    move_throws "double move not from start throws" pf3 "g3g5"
      "Illegal move for a pawn";
    move_throws "illegal diagonal move for pawn throws" pf3 "f2e3"
      "Illegal move for a pawn";
    move_throws "illegal triple move for pawn throws" pf3 "a2a5"
      "Illegal move for a pawn";
    move_throws "white pawn cannot move backwards" pf3 "e5e4"
      "Illegal move for a pawn";
    en_passant_nt "en passant works on black too" pf4 "f4e3" "e3" "e4"
      "p";
    move_throws
      "black pawn cannot move double backwards from white starting line"
      pf4 "g2g4" "Illegal move for a pawn";
    move_throws "black pawn cannot double move through a piece" pf7
      "a7a5" "Illegal move for a pawn";
    move_throws
      "white pawn cannot capture vertically when moving two spaces" pf5
      "c2c4" "Illegal move for a pawn";
    move_throws
      "black pawn cannot capture vertically when moving one space" pf7
      "a7a6" "Pawn cannot take vertically";
    promote_tester "black pawn promotes to black queen" pf4 "a2a1" "Q";
    promote_tester "black pawn promotes to black rook" pf4 "a2a1" "R";
    promote_tester "black pawn promotes to black knight" pf4 "a2a1" "N";
    promote_tester "black pawn promotes to black bishop" pf4 "a2a1" "B";
    promote_throws "promoting to a king doesn't work" pf3 "a2a1" "K"
      "Cannot parse K as a promotable piece";
    check_true "promoting to knight near king causes check"
      promote_fen_alt "c2c1" "N";
    check_true
      "promoting to knight near king on other side causes check"
      promote_fen_alt "a2a1" "N";
    check_false
      "promoting to knight near king without being in L causes no check"
      promote_fen_alt "b2b1" "N";
    check_false
      "promoting to knight to right of king doesn't cause check"
      promote_fen_alt2 "g2g1" "N";
    check_false
      "promoting to bishop in line with king with blocking piece \
       doesn't cause check"
      promote_fen_alt "d2d1" "B";
    check_true
      "promoting to rook in vertical line with king causes check"
      promote_fen_alt "b2b1" "R";
    check_true "promoting to knight right causes check for black king"
      promote_fen_alt3 "f7f8" "N";
    check_true "promoting to knight left causes check for black king"
      promote_fen_alt3 "d7d8" "N";
    check_false "promoting to knight left far does not cause check"
      promote_fen_alt3 "c7c8" "N";
    check_false "promoting to knight right far does not cause check"
      promote_fen_alt3 "g7g8" "N";
    check_true
      "promoting to knight right close L causes check for black king"
      promote_fen_alt4 "g7g8" "N";
    check_true
      "promoting to knight left close L causes check for black king"
      promote_fen_alt4 "c7c8" "N";
    check_true
      "promoting to white knight left close L causes check for white \
       king"
      promote_fen_alt5 "a2a1" "N";
    check_true
      "promoting to white knight right close L causes check for white \
       king"
      promote_fen_alt5 "e2e1" "N";
    promote_tester "white pawn promotes to white queen" pf8 "g7g8" "Q";
    promote_tester "white pawn promotes to white bishop" pf8 "g7g8" "B";
    promote_tester "white pawn promotes to white knight" pf8 "g7g8" "N";
    promote_tester "white pawn promotes to white rook" pf8 "g7g8" "R";
  ]

let whking_castle_kside =
  "rnbqk2r/pppppp1p/5npb/8/8/5NPB/PPPPPP1P/RNBQK2R w KQkq - 0 1 "

let blking_castle_kside =
  "rnbqk2r/pppppp1p/5npb/8/8/5NPB/PPPPPP1P/RNBQK2R b KQkq - 0 1 "

let whking_castle_qside =
  "r3kbnr/ppp1pppp/2nq4/3p1b2/3P1B2/2NQ4/PPP1PPPP/R3KBNR w KQkq - 0 1"

let blking_castle_qside =
  "r3kbnr/ppp1pppp/2nq4/3p1b2/3P1B2/2NQ4/PPP1PPPP/R3KBNR b KQkq - 0 1"

let whking_castle_leads_to_check =
  "r3kbnr/ppp1pppp/2n5/3p1b2/3P1q2/2NQ4/PPP1PPPP/R3KBNR w KQkq - 0 1"

let whqrook_has_moved =
  "r3kbnr/ppp1pppp/2nq4/3p1b2/3P4/2NQ4/PPP1PPPP/1R2KBNR w Kkq - 0 1"

let whkrook_has_moved =
  "rnbqk2r/ppp1p2p/5npb/3p1p2/4P1P1/5P1P/PPPPN1BR/RNBQK3 w Qkq - 0 1"

let b_no_queenside =
  "r3k2r/pppn2pp/4bp2/2Q1p3/4P3/2NB4/PPPP1PPP/R1B1K2R b KQk - 0 1"

let b_no_kingside =
  "r3k2r/pppn2pp/4bp2/3Qp3/4P3/2NB4/PPPP1PPP/R1B1K2R b KQq - 0 1"

let wh_cstl_thr_chk =
  "rnb2k1r/pppQ1ppp/8/2b1p3/2B1P3/2Nq4/PPPP1PPP/R1B1K2R w KQkq - 0 1"

let b_cstl_thr_chk =
  "rnb1k2r/ppp3pp/5p2/2Q1p3/4P3/2NB4/PPPP1PPP/R1B1K2R b KQkq - 0 1"

let wh_castle_out_chk =
  "rnb2k1r/pppQ1ppp/8/2b1p3/2B1P3/2N1q3/PPPP1PPP/R1B1K2R w KQkq - 0 1"

let wh_king_along_chk =
  "rnbq1bnr/pppppkpp/5p2/7Q/4P3/8/PPPP1PPP/RNB1KBNR b KQ - 0 1"

let only_king = "8/8/8/8/4K3/8/8/8 w - - 0 1"

let two_kings = "8/8/8/3k4/8/3K4/8/8 w - - 0 1"

let wh_cstl_take =
  "rnbqkb1r/pppp1ppp/8/4p3/2B3Q1/P1NPB3/1PP2PPP/R3K1nR w KQkq - 0 1"

let wh_no_rook =
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQK3 w KQkq - 0 1"

let castle_check name board move_str expected final_pos1 final_pos2 =
  let pos = move move_str "" (load_fen board) in
  name >:: fun _ ->
  assert_equal
    (Board.get_piece final_pos1 pos ^ Board.get_piece final_pos2 pos)
    expected

let castle_throw name board move_str exn =
  name >:: fun _ ->
  assert_raises exn (fun () -> move move_str "" (load_fen board))

let king_tests =
  [
    castle_check "white king can castle kingside" whking_castle_kside
      "e1g1" "KR" "g1" "f1";
    castle_check "black king can castle kingside" blking_castle_kside
      "e8g8" "kr" "g8" "f8";
    castle_check "white king can castle queenside" whking_castle_qside
      "e1c1" "KR" "c1" "d1";
    castle_check "black king can castle queenside" blking_castle_qside
      "e8c8" "kr" "c8" "d8";
    castle_throw "queenside castle leads to check"
      whking_castle_leads_to_check "e1c1"
      (IllegalMove "Moving this piece would place you in check");
    (* ^ kind of unnecessary*)
    castle_throw "white kign castling through check throws"
      wh_cstl_thr_chk "e1g1"
      (IllegalMove "King cannot castle through check");
    castle_throw "black king castling through check throws"
      b_cstl_thr_chk "e8g8"
      (IllegalMove "King cannot castle through check");
    castle_throw "castling out of check throws" wh_castle_out_chk "e1g1"
      (IllegalMove "King cannot castle out of check");
    castle_throw "castling such that you capture a piece throws"
      wh_cstl_take "e1g1"
      (IllegalMove "White king cannot castle kingside");
    castle_throw
      "castling black king queenside without queenside castling rights \
       throws"
      b_no_queenside "e8c8"
      (IllegalMove "Black king cannot castle queenside");
    castle_throw
      "castling black king kingside without kingside castling rights \
       throws"
      b_no_kingside "e8g8"
      (IllegalMove "Black king cannot castle kingside");
    castle_throw "castling without rook throws" wh_no_rook "e1g1"
      (IllegalMove "White king cannot castle kingside");
    castle_throw "queenside rook has moved" whqrook_has_moved "e1c1"
      (IllegalMove "White king cannot castle queenside");
    castle_throw "kingside rook has moved" whkrook_has_moved "e1g1"
      (IllegalMove "White king cannot castle kingside");
    move_throws "King cannot move two up" only_king "e4e6"
      "King can only move one spot when not castling";
    move_throws "King cannot move two down" only_king "e4e2"
      "King can only move one spot when not castling";
    move_throws "King cannot move two left" only_king "e4c4"
      "King can only move one spot when not castling";
    move_throws "King cannot move two right" only_king "e4g4"
      "King can only move one spot when not castling";
    move_throws "King cannot move two SW" only_king "e4c2"
      "King can only move one spot when not castling";
    move_throws "King cannot move two NW" only_king "e4c6"
      "King can only move one spot when not castling";
    move_throws "King cannot move two NE" only_king "e4g6"
      "King can only move one spot when not castling";
    move_throws "King cannot move two SE" only_king "e4g2"
      "King can only move one spot when not castling";
    move_throws "King cannot move like a knight" only_king "e4c3"
      "Illegal move for a king";
    move_throws "King cannot move backwards along a check"
      wh_king_along_chk "f7e8" "Invalid move, you are in check!";
    move_no_throw "King can move up one" only_king "e4e5" "e5" "K";
    move_no_throw "King can move down one" only_king "e4e3" "e3" "K";
    move_no_throw "King can move one NE" only_king "e4f5" "f5" "K";
    move_no_throw "King can move one SE" only_king "e4f3" "f3" "K";
    move_no_throw "King can move one left" only_king "e4d4" "d4" "K";
    move_throws "White king cannot move up towards black king" two_kings
      "d3d4" "King cannot move adjacent to enemy king";
    move_throws "White king cannot move NW towards black king" two_kings
      "d3c4" "King cannot move adjacent to enemy king";
  ]

(**bishop FENs and movestrings for is_check tests*)
let b1 = "rnbqkbnr/pp2p1pp/8/2pP4/3P4/8/PPP1BPPP/RNBQK1NR w KQkq - 0 3"

let b2 =
  "rnbqkbnr/pp4pp/4p3/3P4/2p5/3P1P2/PPP1B1PP/RNBQ1RK1 b Qkq - 0 3"

let b3 =
  "rnbqkbnr/p5pp/1p2p3/2p5/3P4/3P2P1/PPP1BP1P/RNBQ1R1K b Qkq - 0 3"

let b4 =
  "rn1qkbnr/p2b2pp/1p6/2pK4/3P4/3P2P1/PPP1BP1P/RNBQ1R2 b kq - 0 3"

let b5 = "rn2kbnr/p5pp/1p6/2pK4/3P4/3b2P1/PPP1BP1P/RNBQ1R2 b kq - 0 3"

let b6_fen = "8/8/8/8/2b5/8/4R3/2K5 w - - 0 1"

let b7_fen = "8/8/8/8/5b2/1N6/3R4/2K5 w - - 0 1"

let b8_fen = "8/8/8/8/8/1N6/3R4/2Kb4 w - - 0 1"

let bishop_check_tests =
  [
    check_true "bottom right white bishop checks successfully" b1 "e2h5"
      "Q";
    check_true "bottom left white bishop checks successfully" b1 "e2b5"
      "Q";
    check_true "top left black bishop checks successfully" b2 "f8c5" "Q";
    check_true "top left black bishop light squares checks successfully"
      b3 "c8b7" "Q";
    check_true
      "top left black bishop one square away checks successfully" b4
      "d7c6" "Q";
    check_true
      "top right black bishop one square away checks successfully" b4
      "d7e6" "Q";
    check_true
      "bottom right black bishop one square away checks successfully" b5
      "d3e4" "Q";
    check_true
      "bottom left black bishop one square away checks successfully" b5
      "d3c4" "Q";
    check_false "bishop cannot check vertically" b6_fen "e2e3" "Q";
    check_false "bishop cannot check through pieces" b7_fen "b3d4" "Q";
    check_false "bishop cannot check horizontally" b8_fen "b3d4" "Q";
  ]

(*knight FENs and movestrings for is_check tests*)
let right = "7K/8/8/2ppp3/2pkp2N/2ppp3/8/8 w - - 0 1"

let left = "7k/8/8/3PPP2/n2PKP2/3PPP2/8/8 b - - 0 1"

let top = "4n2k/8/8/3PPP2/3PKP2/3PPP2/8/8 b - - 0 1"

let bottom = "7K/8/2ppp3/2pkp3/2ppp3/8/8/3N4 w - - 0 1"

let n1_fen = "5bk1/5R2/8/8/8/1N2n3/8/2K5 w - - 0 1"

let knight_check_tests =
  [
    check_true "right top middle white knight checks black king" right
      "h4f5" "Q";
    check_true "right bottom middle white knight checks black king"
      right "h4f3" "Q";
    check_true "left top middle black knight checks white king" left
      "a4c5" "Q";
    check_true "left bottom middle black knight checks white king" left
      "a4c3" "Q";
    check_true "top right black knight checks white king" top "e8f6" "Q";
    check_true "top left black knight checks white king" top "e8d6" "Q";
    check_true "bottom right white knight checks black king" bottom
      "d1e3" "Q";
    check_true "bottom left white knight checks black king" bottom
      "d1c3" "Q";
    check_false
      "knight does not check an extra square away horizontally or \
       vertically"
      n1_fen "b3c5" "Q";
  ]

(*pawn FENs and movestrings for is_check tests*)
let w1 = "rn1q1bnr/pp2pppp/2p5/5b2/1k1PN3/8/PPP2PPP/R1BQKBNR w KQ - 1 5"

let b1 = "rn1kqbnr/pp1ppppp/2p5/4Kb2/3PN3/8/PPP2PPP/R1BQ1BNR b kq - 1 5"

let dw =
  "rn1k1bnr/pp1ppppp/2p5/1k2qb2/3PN3/8/PPP2PPP/R1BQKNNR w KQ - 1 5"

let w2 = "rn1q1bnr/pp2pppp/2p5/5b2/PkPPN3/8/1P3PPP/R1BQKBNR w KQ - 1 5"

let pawn_check_tests =
  [
    check_true "right white pawn check works" w1 "c2c3" "Q";
    check_true "left white pawn check works" w1 "a2a3" "Q";
    check_true "right black pawn check works" b1 "d7d6" "Q";
    check_true "left black pawn check works" b1 "f7f6" "Q";
    check_true "double move white pawn check works" dw "c2c4" "Q";
    check_false
      "pawn next to and diagonal ahead right of black king does not \
       cause check"
      w2 "c4c5" "Q";
    check_false
      "pawn next to and diagonal ahead left of black king does not \
       cause check"
      w2 "a4a5" "Q";
  ]

(*rook FENs and movestrings for is_check tests*)
let r1 = "5k2/3R4/6p1/7R/7p/7r/1r4P1/4K3 b - - 0 1"

let r2 = "5k2/3R4/6p1/7R/7p/7r/1r4P1/4K3 w - - 0 1"

let r3 = "8/3R4/6pR/1r3k2/3K3p/7r/6P1/8 b - - 0 1"

let r4 = "8/3R4/6p1/1r3k1R/3K3p/7r/6P1/8 w - - 0 1"

let r5_fen = "6k1/6b1/3n4/8/6R1/1N6/8/2K5 b - - 0 1"

let r6_fen = "1R3bk1/8/3n4/8/8/1N6/8/2K5 b - - 0 1"

let r7_fen = "5bk1/5R2/3n4/8/8/1N6/8/2K5 b - - 0 1"

let rook_check_tests =
  [
    check_true "left black rook checks correctly" r1 "b2b1" "Q";
    check_true "left black rook checks correctly" r1 "h3h1" "Q";
    check_true "top black rook checks correctly" r1 "b2e2" "Q";
    check_true "right white rook checks correctly" r2 "h5h8" "Q";
    check_true "left white rook checks correctly" r2 "d7d8" "Q";
    check_true "bottom white rook checks correctly" r2 "d7f7" "Q";
    check_true "top black rook checks correctly" r3 "b5d5" "Q";
    check_true "top white rook checks correctly" r4 "d7f7" "Q";
    check_false "rook cannot check through pieces vertically" r5_fen
      "d6c4" "Q";
    check_false "rook cannot check through pieces horizontally" r6_fen
      "d6c4" "Q";
    check_false "rook cannot check diagonally" r7_fen "d6c4" "Q";
  ]

(*FENs for discovery check tests*)
let disc_fen1 = "8/1K2b1q1/PP6/8/8/N7/8/8 b - - 0 1"

let disc_fen2 = "8/1K6/PP6/3n4/8/5b2/8/8 b - - 0 1"

let disc_fen3 = "8/8/8/8/P7/1P1N4/1K2p2r/8 b - - 0 1"

let discovery_check_tests =
  [
    check_true "moving bishop out of queen attack checks king" disc_fen1
      "e7a3" "Q";
    check_true "moving knight out of bishop attack causes check"
      disc_fen2 "d5e7" "Q";
    check_true "moving pawn out of rook line causes check" disc_fen3
      "e2e1" "Q";
  ]

let move_blocks_check name board mv expected =
  let board = move mv "" board in
  name >:: fun _ -> assert_equal expected (is_in_check board)

let block_check_fen =
  "r2qkbnr/pppp2pp/B2nbp2/4p2Q/4P3/2N5/PPPP1PPP/R1B1K1NR b KQkq - 0 1"

let block_check_tests =
  [
    move_blocks_check
      "putting pawn in front of queen check results in no check"
      (load_fen block_check_fen)
      "g7g6" false;
    move_blocks_check
      "putting bishop in front of queen check results in no check"
      (load_fen block_check_fen)
      "e6f7" false;
    move_blocks_check
      "putting knight in front of queen check results in no check"
      (load_fen block_check_fen)
      "d6f7" false;
  ]

let check_tests =
  List.flatten
    [
      bishop_check_tests;
      knight_check_tests;
      rook_check_tests;
      pawn_check_tests;
      block_check_tests;
    ]

(*Positions for bishop pins*)
let bishop_pin1 = "8/8/6b1/8/8/3N4/8/1K6 w - - 0 1"

let bishop_pin2 = "8/8/2b5/8/8/5N2/8/7K w - - 0 1"

let bishop_pin3 = "8/8/5K2/8/3N4/8/1b6/8 w - - 0 1"

let bishop_pin4 = "8/8/1k6/8/3n4/8/5B2/8 b - - 0 1"

(*Positions for rook pins*)
let rook_pin1 = "8/8/8/8/8/8/5PPP/3r1N1K w - - 0 1"

let rook_pin2 = "8/8/8/8/P1P5/1P6/1KN4r/8 w - - 0 1"

let rook_pin3 = "8/7r/2k1p3/3p4/2n5/8/5K2/2R5 b - - 0 1"

let rook_pin4 = "8/5k1r/2b1p3/2Kp1q2/8/8/8/5R2 b - - 0 1"

(*Positions for queen pins*)
let qpin1 = "8/5k1r/2b1p3/2Kp1q2/8/8/8/5Q2 b - - 0 1"

let qpin2 = "8/7r/2k1p3/3p4/2n5/8/5K2/2Q5 b - - 0 1"

let qpin3 = "8/8/8/8/P7/1P6/1KP4q/8 w - - 0 1"

let qpin4 = "8/8/8/8/8/5P2/6PP/3q1R1K w - - 0 1"

let qpin5 = "8/8/1k6/8/3b4/8/5QK1/8 b - - 0 1"

let qpin6 = "8/8/5K2/8/3R4/8/1q6/8 w - - 0 1"

let qpin7 = "8/8/2q5/8/8/5N2/8/7K w - - 0 1"

let qpin8 = "8/8/6q1/8/8/3Q4/8/1K6 w - - 0 1"

let pin_tests =
  [
    move_throws "top right black bishop pin light squares raises"
      bishop_pin1 "d3b4" "Moving this piece would place you in check";
    move_throws "top left black bishop pin light squares raises"
      bishop_pin2 "f3g5" "Moving this piece would place you in check";
    move_throws "bottom left white bishop pin dark squares raises"
      bishop_pin3 "d4f3" "Moving this piece would place you in check";
    move_throws "bottom right white bishop pin dark squares raises"
      bishop_pin4 "d4e6" "Moving this piece would place you in check";
    move_throws "left side horizontal black rook raises" rook_pin1
      "f1e3" "Moving this piece would place you in check";
    move_throws "right side horizontal black rook raises" rook_pin2
      "c2d4" "Moving this piece would place you in check";
    move_throws "bottom vertical white rook raises" rook_pin3 "c4d3"
      "Moving this piece would place you in check";
    move_throws "top vertical white rook raises" rook_pin4 "f5c2"
      "Moving this piece would place you in check";
    move_throws "top vertical white queen raises" qpin1 "f5c2"
      "Moving this piece would place you in check";
    move_throws "bottom vertical white queen raises" qpin2 "c4d2"
      "Moving this piece would place you in check";
    move_throws "right horizontal black queen raises" qpin3 "c2c3"
      "Moving this piece would place you in check";
    move_throws "left horizontal black queen raises" qpin4 "f1f2"
      "Moving this piece would place you in check";
    move_no_throw
      "left horizontal black queen can be captured without throwing"
      qpin4 "f1d1" "d1" "R";
    move_throws "bottom right white queen raises" qpin5 "d4e5"
      "Moving this piece would place you in check";
    move_no_throw
      "bottom right white queen can be captured without throwing" qpin5
      "d4f2" "f2" "b";
    move_throws "bottom left black queen raises" qpin6 "d4e4"
      "Moving this piece would place you in check";
    move_throws "top right white queen raises" qpin7 "f3g5"
      "Moving this piece would place you in check";
    move_throws "top right white queen raises" qpin8 "d3d8"
      "Moving this piece would place you in check";
  ]

let fullmove_clock_tester name expected board =
  name >:: fun _ -> assert_equal expected (Board.fullmove_clock board)

let checked_state = "4R3/2Q5/P7/P6k/5b2/6p1/6K1/5B1q w - - 0 1"

let pawn_moves_straight =
  "r1bq4/4b1p1/ppk5/P2pP3/1p1P1K2/P1P4N/1BR4P/RN6 w - - 0 1"

let fullmove_board =
  move_list
    [
      ("e2e4", "");
      ("e7e5", "");
      ("g1f3", "");
      ("b8c6", "");
      ("f1c4", "");
      ("g8f6", "");
      ("b1c3", "");
      ("f8d6", "");
      ("d1e2", "");
      ("h8f8", "");
      ("d2d3", "");
      ("a7a5", "");
    ]
    (init ())

let general_move_tests =
  [
    move_throws "moving piece to its current square throws" qpin8 "g6g6"
      "Cannot move piece to the square it is currently at";
    move_throws "trying to move from an empty square throws" qpin8
      "a8b8" "a8b8 does not contain a valid from square";
    move_throws "Can't move any piece but the king" checked_state "e8e7"
      "Invalid move, you are in check!";
    check_false "Pawn moving forward shouldn't check king"
      pawn_moves_straight "c3c4" "Q";
    move_throws "white cannot move a black piece" pawn_moves_straight
      "g7g6" "White does not own the piece on g7";
    fullmove_clock_tester
      "doing a sequence of moves from the starting position results in \
       the correct fullmove clock"
      13 fullmove_board;
  ]

let move_tests =
  List.flatten
    [
      knight_tests;
      bishop_tests;
      rook_tests;
      pawn_tests;
      queen_tests;
      pin_tests;
      king_tests;
      general_move_tests;
    ]

let seq_equals name move_seq1 move_seq2 expected =
  let pos1 = move_list move_seq1 (init ()) in
  let pos2 = move_list move_seq2 (init ()) in
  name >:: fun _ -> assert_equal expected (Board.equals pos1 pos2)

let equals_tests =
  [
    seq_equals
      "castling opening sequences are equal even if move ordering is \
       different"
      [
        ("e2e4", "");
        ("e7e5", "");
        ("g1f3", "");
        ("b8c6", "");
        ("f1c4", "");
        ("g8f6", "");
        ("b1c3", "");
        ("f8d6", "");
        ("d1e2", "");
        ("h8f8", "");
        ("d2d3", "");
        ("a7a5", "");
        ("e1g1", "");
      ]
      [
        ("e2e4", "");
        ("e7e5", "");
        ("f1c4", "");
        ("f8d6", "");
        ("b1c3", "");
        ("g8f6", "");
        ("d1e2", "");
        ("h8f8", "");
        ("g1f3", "");
        ("b8c6", "");
        ("d2d3", "");
        ("a7a5", "");
        ("e1g1", "");
      ]
      true;
  ]

let gen_random x =
  Random.self_init ();
  Random.int x

let random_move board () =
  let d = move_generator board in
  let a = gen_random (List.length d) in
  List.nth d a

let rec random_game board x =
  if x <= 0 then board
  else if checkmate board || draw board then board
  else
    let a = random_move board () in
    let b = move a "Q" board in
    random_game b (x - 1)

let rec undo_seq_compare undo_board fen move_seq =
  match List.rev move_seq with
  | [] -> true
  | h :: t ->
      let fresh_init = load_fen fen in
      let next_undo = Board.undo_prev undo_board in
      let new_board = move_list (List.rev t) fresh_init in
      if Board.equals new_board next_undo then
        undo_seq_compare next_undo fen (List.rev t)
      else false

let undo_seq_tester name fen move_seq =
  let board = load_fen fen in
  let final_board = move_list move_seq board in
  let is_equal_seq = undo_seq_compare final_board fen move_seq in
  name >:: fun _ ->
  assert_bool
    "Undoing did not result in the same board as the original move \
     sequence"
    is_equal_seq

let undo_seq_bool name move_seq =
  let final_board = move_list move_seq (init ()) in
  undo_seq_compare final_board init_fen move_seq

let undo_random_tester name num =
  let rec undo_helper num =
    if num = 0 then true
    else
      let this_board = random_game (init ()) 100 in
      undo_seq_bool name (get_moves this_board) && undo_helper (num - 1)
  in
  let all_equal = undo_helper num in
  name >:: fun _ ->
  assert_bool "some of the random games were not equal while undoing"
    all_equal

let undo_throws name board mv mv2 mv3 =
  let board = move mv "" board in
  let board = move mv2 "" board in
  move mv3 "" board;
  name >:: fun _ ->
  assert_raises
    (IllegalMove
       "Attempted to undo a move with a piece that wasn't there")
    (fun () -> board |> undo_prev |> undo_prev)

let capture_seq =
  [
    ("e2e4", "");
    ("e7e5", "");
    ("g1f3", "");
    ("b8c6", "");
    ("f1c4", "");
    ("g8f6", "");
    ("b1c3", "");
    ("f8d6", "");
    ("d1e2", "");
    ("h8f8", "");
    ("d2d3", "");
    ("a7a5", "");
    ("f3e5", "");
    ("d6e5", "");
    ("c3d5", "");
    ("f6d5", "");
    ("c4d5", "");
    ("d8h4", "");
    ("d5c6", "");
    ("b7c6", "");
    ("c1e3", "");
    ("e5h2", "");
    ("h1h2", "");
    ("h4h2", "");
    ("e2g4", "");
    ("h2h1", "");
    ("e1d2", "");
    ("h1a1", "");
    ("g4g7", "");
    ("a1a2", "");
  ]

let undo_move_tests =
  [
    undo_seq_tester
      "undoing every move in the opening with no captures results in \
       the same board as the initial move sequence"
      init_fen
      [
        ("e2e4", "");
        ("e7e5", "");
        ("g1f3", "");
        ("b8c6", "");
        ("f1c4", "");
        ("g8f6", "");
        ("b1c3", "");
        ("f8d6", "");
        ("d1e2", "");
        ("h8f8", "");
        ("d2d3", "");
        ("a7a5", "");
      ];
    undo_seq_tester
      "undoing initial move sequence with en passant works" init_fen
      [
        ("e2e4", "");
        ("e7e5", "");
        ("g1f3", "");
        ("b8c6", "");
        ("f1c4", "");
        ("g8f6", "");
        ("b1c3", "");
        ("f8d6", "");
        ("d1e2", "");
        ("h8f8", "");
        ("d2d3", "");
        ("a7a5", "");
        ("g2g4", "");
        ("b7b6", "");
        ("g4g5", "");
        ("h7h5", "");
        ("g5h6", "");
        ("g7g5", "");
        ("e2e3", "");
        ("g5g4", "");
        ("h2h4", "");
        ("g4h3", "");
      ];
    undo_seq_tester
      "undoing initial move sequence with queenside castling works"
      init_fen
      [
        ("e2e4", "");
        ("e7e5", "");
        ("b1c3", "");
        ("b8c6", "");
        ("d2d3", "");
        ("d7d6", "");
        ("c1d2", "");
        ("c8e6", "");
        ("d1e2", "");
        ("d8e7", "");
        ("h2h3", "");
        ("e8c8", "");
        ("g1f3", "");
      ];
    undo_seq_tester
      "undoing initial move sequence with kingside castling works"
      init_fen
      [
        ("e2e4", "");
        ("e7e5", "");
        ("g1f3", "");
        ("b8c6", "");
        ("f1c4", "");
        ("g8f6", "");
        ("b1c3", "");
        ("f8d6", "");
        ("d1e2", "");
        ("h8f8", "");
        ("d2d3", "");
        ("a7a5", "");
        ("e1g1", "");
      ];
    undo_seq_tester
      "undoing every move in the opening with a variety of captures  \
       correctly matches initial move sequence"
      init_fen capture_seq;
    undo_seq_tester
      "undoing position with no moves returns same position" init_fen [];
    undo_seq_tester
      "undoing every move in a sequence of promotions updates board \
       correctly"
      promote_fen
      [
        ("a2a1", "Q");
        ("e4e5", "");
        ("b2b1", "N");
        ("e5e6", "");
        ("c2c1", "B");
        ("e6e7", "");
        ("d2e1", "R");
      ];
    undo_throws
      "undoing after mutating the board separately, moving the piece \
       to undo, throws an error"
      (init ()) "e2e4" "a7a5" "e4e5";
    undo_random_tester
      "Performing 60 random moves on a board and then undoing each \
       individually to compare with the normally generated board \
       results in all equivalent boards"
      1000;
  ]

let fen_test name board colid expected =
  let col = make_col board colid in
  name >:: fun _ -> assert_equal col expected

let fen_turn name (board : Board.t) expected =
  name >:: fun _ -> assert_equal expected (get_turn board)

let fen_is_check name board expected =
  name >:: fun _ ->
  assert_equal expected (is_in_check board) ~printer:(fun x ->
      if x then "true" else "false")

let is_draw_tester name board expected =
  name >:: fun _ ->
  assert_equal expected (draw board) ~printer:(fun x ->
      if x then "true" else "false")

let fen_equals_board name board move_seq =
  let move_board = move_list move_seq (init ()) in
  name >:: fun _ ->
  assert_bool "boards were not equal" (Board.equals board move_board)

let fen_throws name fen error =
  name >:: fun _ ->
  assert_raises (IllegalFen error) (fun () -> load_fen fen)

let fen = "k6r/2qP4/3n1bPn/1r4p1/2B1P3/BNP2N2/3R3P/1QKb3R w - - 0 1"

let fen_check =
  "rnbqkbnr/ppppp2p/5p2/6pQ/4P3/8/PPPP1PPP/RNB1KBNR b - - 0 1"

let fen_not_check = "8/3R4/6p1/1r3k1R/3K3p/7r/6P1/8 w - - 0 1"

let fen_fiftyfold_draw =
  "r1b1kr2/1ppp1ppp/2P5/p3b3/2B4q/3P4/PPP1QPPP/R1B1K2R w KQq - 100 1"

let captures_fen =
  "r1b1kr2/2pp1pQp/2p5/p7/4P3/3PB3/qPPK1PP1/8 w q - 0 1"

let captures_board = load_fen captures_fen

let fen_board = load_fen fen

let check_board = load_fen fen_check

let not_check_board = load_fen fen_not_check

let fiftyfold_draw_board = load_fen fen_fiftyfold_draw

let fen_tests =
  [
    fen_test "First col of FEN is NANABNANANANAk" fen_board "a"
      "NANABNANANANAk";
    fen_test "Second col of FEN is QNANNArNANANA" fen_board "b"
      "QNANNArNANANA";
    fen_test "Third col of FEN is KNAPBNANAqNA" fen_board "c"
      "KNAPBNANAqNA";
    fen_test "Fourth col of FEN is bRNANANAnPNA" fen_board "d"
      "bRNANANAnPNA";
    fen_test "Fifth col of FEN is NANANAPNANANANA" fen_board "e"
      "NANANAPNANANANA";
    fen_test "Sixth col of FEN is NANANNANAbNANA" fen_board "f"
      "NANANNANAbNANA";
    fen_test "Seventh col of FEN is NANANANApPNANA" fen_board "g"
      "NANANANApPNANA";
    fen_test "Eighth col of FEN is RPNANANAnNAr" fen_board "h"
      "RPNANANAnNAr";
    fen_turn "FEN stores correct turn" fen_board true;
    fen_is_check "First fen is not check" fen_board false;
    fen_is_check "basic check is recognized by fen" check_board true;
    fen_is_check "basic endgame non-check recognized by fen"
      not_check_board false;
    is_draw_tester
      "loading a fen with a full halfmove clock results in a draw"
      fiftyfold_draw_board true;
    fen_equals_board
      "loading a fen equivalent to a move sequence with multiple \
       captures is equivalent"
      captures_board capture_seq;
    fen_throws "fen without en passant throws"
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq"
      "FEN does not contain en passant information";
    fen_throws "fen without halfmove clock throws"
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -"
      "FEN does not contain halfmove clock";
    fen_throws "fen without fullmove clock throws"
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0"
      "FEN does not contain fullmove clock";
    fen_throws "fen with illegal symbols throws"
      "rnbqkbnr/ppp$pppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      "$ is not a valid FEN number or symbol";
    fen_throws "fen with illegal letter throws"
      "rnbqkbnr/appppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      "a is not a valid FEN letter";
  ]

let move_gen_tester name board expected =
  let pos = load_fen board in
  name >:: fun _ ->
  assert_equal
    (List.sort
       (fun x y -> if x > y then 1 else if x < y then -1 else 0)
       (move_generator pos))
    expected

let initial_board =
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let init_moves =
  [
    "a2a3";
    "a2a4";
    "b1a3";
    "b1c3";
    "b2b3";
    "b2b4";
    "c2c3";
    "c2c4";
    "d2d3";
    "d2d4";
    "e2e3";
    "e2e4";
    "f2f3";
    "f2f4";
    "g1f3";
    "g1h3";
    "g2g3";
    "g2g4";
    "h2h3";
    "h2h4";
  ]

let cstl_board =
  "r1b1k1nr/ppppq1pp/8/b2QP3/8/2p2N2/P4PPP/RN2K2R w KQkq - 0 1"

let cstl_moves =
  [
    "a2a3";
    "a2a4";
    "b1a3";
    "b1c3";
    "b1d2";
    "d5a5";
    "d5b3";
    "d5b5";
    "d5b7";
    "d5c4";
    "d5c5";
    "d5c6";
    "d5d1";
    "d5d2";
    "d5d3";
    "d5d4";
    "d5d6";
    "d5d7";
    "d5e4";
    "d5e6";
    "d5f7";
    "d5g8";
    "e1d1";
    "e1e2";
    "e1f1";
    "e1g1";
    "e5e6";
    "f3d2";
    "f3d4";
    "f3g1";
    "f3g5";
    "f3h4";
    "g2g3";
    "g2g4";
    "h1f1";
    "h1g1";
    "h2h3";
    "h2h4";
  ]

let psnt_board = "N1b2rk1/pp3qpp/7n/Q2pP3/P7/5N1P/5PP1/R4RK1 w Q d6 0 1"

let psnt_moves =
  [
    "a1a2";
    "a1a3";
    "a1b1";
    "a1c1";
    "a1d1";
    "a1e1";
    "a5a6";
    "a5a7";
    "a5b4";
    "a5b5";
    "a5b6";
    "a5c3";
    "a5c5";
    "a5c7";
    "a5d2";
    "a5d5";
    "a5d8";
    "a5e1";
    "a8b6";
    "a8c7";
    "e5d6";
    "e5e6";
    "f1b1";
    "f1c1";
    "f1d1";
    "f1e1";
    "f3d2";
    "f3d4";
    "f3e1";
    "f3g5";
    "f3h2";
    "f3h4";
    "g1h1";
    "g1h2";
    "g2g3";
    "g2g4";
    "h3h4";
  ]

  (*Used the following python code to generate the legal moves for each board:
  import chess
  def lst_from_brd(board):
      lg = board.legal_moves
      p = []
      for i in lg:
          p.append(i.uci())
      p.sort()
      return p
  board = chess.Board([FEN STRING HERE])
  lst_from_brd(board)
  *)[@@ocamlformat "disable"]

(**[move_gen_random name] creates an OUnit test that always returns
   true, contingent on the creation of a random game with 60 moves not
   throwing an error from an illegal move being generated *)
let move_gen_random name num =
  let all_ran =
    let rec move_gen_random_helper num =
      if num = 0 then true
      else
        let board = random_game (init ()) 60 in
        move_gen_random_helper (num - 1)
    in
    move_gen_random_helper num
  in
  name >:: fun _ ->
  assert_bool
    "This cannot fail; the test exists to assert that no illegal moves \
     were thrown"
    all_ran

let move_gen_tests =
  [
    move_gen_tester
      "initial position contains all legal moves when generated"
      initial_board init_moves;
    move_gen_tester
      "more complicated position with castling contains all legal moves"
      cstl_board cstl_moves;
    move_gen_tester
      "en passant complex position contains all legal moves" psnt_board
      psnt_moves;
    move_gen_random
      "running 20 random games with the move generator throws no \
       errors from illegal moves being generated"
      20;
  ]

let draw_tester name board moves expected =
  let board = load_fen board in
  let board = move_list moves board in
  name >:: fun _ ->
  assert_equal expected (draw board) ~printer:(fun x ->
      if x then "true" else "false")

let rmv_last lst =
  match List.rev lst with [] -> [] | h :: t -> List.rev t

let king_material_draw = "r6k/n7/8/8/8/8/7N/K6R w - - 0 1"

let hundred_distinct_moves =
  [
    ("a1a2", "");
    ("h8h7", "");
    ("a2a3", "");
    ("h7h6", "");
    ("a3a4", "");
    ("h6h5", "");
    ("a4a5", "");
    ("h5h4", "");
    ("a5a6", "");
    ("h4h3", "");
    ("a6b6", "");
    ("h3g3", "");
    ("b6c5", "");
    ("g3f4", "");
    ("c5c4", "");
    ("f4f5", "");
    ("c4c3", "");
    ("f5f6", "");
    ("c3c2", "");
    ("f6f7", "");
    ("c2c1", "");
    ("f7f8", "");
    ("c1d1", "");
    ("f8g8", "");
    ("d1d2", "");
    ("g8g7", "");
    ("d2d3", "");
    ("g7g6", "");
    ("d3d4", "");
    ("g6g5", "");
    ("d4d5", "");
    ("g5f4", "");
    ("d5d6", "");
    ("f4e3", "");
    ("d6d7", "");
    ("e3d2", "");
    ("d7e7", "");
    ("d2c2", "");
    ("e7f7", "");
    ("c2b2", "");
    ("f7g7", "");
    ("b2a2", "");
    ("g7h7", "");
    ("a2a3", "");
    ("h7h6", "");
    ("a3a4", "");
    ("h6h5", "");
    ("a4a5", "");
    ("h5h4", "");
    ("a5a6", "");
    ("h4h3", "");
    ("a6b7", "");
    ("h3g2", "");
    ("b7b8", "");
    ("g2g1", "");
    ("b8c8", "");
    ("g1f1", "");
    ("c8d8", "");
    ("f1e1", "");
    ("d8e8", "");
    ("e1d1", "");
    ("e8f8", "");
    ("d1c1", "");
    ("f8g8", "");
    ("c1b1", "");
    ("g8h8", "");
    ("b1a1", "");
    ("h8h7", "");
    ("a1a2", "");
    ("h7h6", "");
    ("a2a3", "");
    ("h6h5", "");
    ("a3a4", "");
    ("h5h4", "");
    ("a4a5", "");
    ("h4h3", "");
    ("a5a6", "");
    ("h3g3", "");
    ("a6b6", "");
    ("g3f4", "");
    ("b6c5", "");
    ("f4f5", "");
    ("c5c4", "");
    ("f5f6", "");
    ("c4c3", "");
    ("f6f7", "");
    ("c3c2", "");
    ("f7f8", "");
    ("c2c1", "");
    ("f8g8", "");
    ("c1d1", "");
    ("g8g7", "");
    ("d1d2", "");
    ("g7g6", "");
    ("d2d3", "");
    ("g6g5", "");
    ("d3d4", "");
    ("g5f4", "");
    ("d4d5", "");
    ("f4e3", "");
  ]

let bongcloud_moves =
  [
    ("e2e4", "");
    ("e7e5", "");
    ("e1e2", "");
    ("e8e7", "");
    ("e2e1", "");
    ("e7e8", "");
    ("e1e2", "");
    ("e8e7", "");
    ("e2e1", "");
    ("e7e8", "");
    ("e1e2", "");
    ("e8e7", "");
  ]

let threedraw_fen = "n6R/r7/3k4/8/8/8/8/6NK w - - 0 1"

let threedraw =
  [
    ("h1g2", "");
    ("d6d5", "");
    ("g2h2", "");
    ("d5e6", "");
    ("h2h1", "");
    ("e6d6", "");
    ("g1f3", "");
    ("a8b6", "");
    ("f3g1", "");
    ("b6a8", "");
  ]

let insuf_draw = "k2n2r1/8/8/8/8/8/B7/1K6 w - - 0 1"

let insuf_draw2 = "k7/8/8/8/7b/8/B7/1K2R3 b - - 0 1"

let insuf_draw3 = "k7/8/8/8/8/8/B2r4/2K5 w - - 0 1"

let insuf_draw4 = "3k3K/7p/8/8/R7/8/2b5/8 b - - 0 1"

let insuf_draw5 = "3kn3/8/8/8/8/8/8/KN6 w - - 0 1"

let insuf_draw6 = "3kb3/8/8/8/8/8/8/KN6 w - - 0 1"

let insuf_draw7 = "3kn3/8/8/8/8/8/8/K7 w - - 0 1"

let insuf_draw8 = "3k4/8/8/8/8/8/8/KN6 b - - 0 1"

let insuf_draw9 = "3k4/8/8/8/8/8/8/K7 w - - 0 1"

let insuf_draw10 = "3k4/4n3/8/8/8/8/N7/KN6 w - - 0 1"

let draw_tests =
  [
    draw_tester
      "moving kings across empty board results in fifty fold draw"
      king_material_draw hundred_distinct_moves true;
    draw_tester
      "99 moves without capture or pawn move is not a draw by \
       fiftyfold rule"
      king_material_draw
      (rmv_last hundred_distinct_moves)
      false;
    draw_tester "bongcloud threefold repetition is a draw" init_fen
      bongcloud_moves true;
    draw_tester "bongcloud without last move is not a draw" init_fen
      (rmv_last bongcloud_moves)
      false;
    draw_tester "triangular king movement results in draw" threedraw_fen
      threedraw true;
    draw_tester
      "triangular king movement without last move is not a draw"
      threedraw_fen (rmv_last threedraw) false;
    draw_tester
      "capturing rook leaving insufficient material results in a draw"
      insuf_draw [ ("a2g8", "") ] true;
    draw_tester "not capturing rook results in not having a draw"
      insuf_draw [ ("a2b3", "") ] false;
    draw_tester
      "capturing last rook with a bishop leaving bishop bishop endgame \
       is a draw"
      insuf_draw2 [ ("h4e1", "") ] true;
    draw_tester "bishop and rook against bishop is not a draw"
      insuf_draw2 [ ("h4g3", "") ] false;
    draw_tester "capturing rook leaving BK vs k is a draw" insuf_draw3
      [ ("c1d2", "") ] true;
    draw_tester "BK vs RK is not a draw" insuf_draw3 [ ("c1b1", "") ]
      false;
    draw_tester
      "capturing last pawn with insufficient other material is draw"
      insuf_draw4
      [ ("c2a4", ""); ("h8h7", "") ]
      true;
    draw_tester "not capturing the last pawn in bpk vs K is not a draw"
      insuf_draw4
      [ ("c2a4", ""); ("h8g7", "") ]
      false;
    draw_tester "sequence with promotions is not draw" pf4
      [ ("a2a1", "Q") ] false;
    draw_tester "KN vs kn is a draw" insuf_draw5 [] true;
    draw_tester "KN vs kb is a draw" insuf_draw6 [] true;
    draw_tester "K vs kn is a draw" insuf_draw7 [] true;
    draw_tester "KN vs k is a draw" insuf_draw8 [] true;
    draw_tester "K vs k is a draw" insuf_draw9 [] true;
    draw_tester "KNN vs kn is not a draw" insuf_draw10 [] false;
  ]

let to_fen_tester name fen =
  let board = load_fen fen in
  name >:: fun _ ->
  assert_equal fen (to_fen board) ~printer:(fun x -> x)

let to_fen_tests =
  [
    to_fen_tester "initial board translates to fen correctly" init_fen;
    to_fen_tester
      "complex board with en passant and different castling translates \
       to fen correctly"
      psnt_board;
    to_fen_tester "captures fen translates back to fen correctly"
      captures_fen;
    to_fen_tester "pf6 translates to fen correctly" pf6;
    to_fen_tester "rf1 translates to fen correctly" rf1;
    to_fen_tester "r7_fen translates to fen correctly" r7_fen;
  ]

let revert_tester name expected board i =
  let final_board = revert_prev board i in
  name >:: fun _ ->
  assert_bool "Reverted board and normal board were not equal"
    (Board.equals final_board expected)

let reverted_board =
  move_list
    [ ("e2e4", ""); ("e7e5", ""); ("g1f3", ""); ("b8c6", "") ]
    (init ())

let revert_board =
  move_list
    [
      ("e2e4", "");
      ("e7e5", "");
      ("g1f3", "");
      ("b8c6", "");
      ("f1c4", "");
      ("g8f6", "");
      ("b1c3", "");
      ("f8d6", "");
      ("d1e2", "");
      ("h8f8", "");
      ("d2d3", "");
      ("a7a5", "");
    ]
    (init ())

let revert_tests =
  [
    revert_tester
      "reverting an initial move sequence to the 4th move creates the \
       same board as the initial 4 move sequence"
      reverted_board revert_board 8;
  ]

let board_tests =
  List.flatten
    [
      init_tests;
      get_piece_tests;
      get_pieces_tests;
      to_string_tests;
      move_tests;
      undo_move_tests;
      check_tests;
      fen_tests;
      move_gen_tests;
      equals_tests;
      draw_tests;
      to_fen_tests;
      revert_tests;
    ]

let suite =
  "test suite for chess engine & game" >::: List.flatten [ board_tests ]

let _ = run_test_tt_main suite
