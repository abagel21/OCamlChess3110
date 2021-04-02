open OUnit2
open Board
open Piece

let make_col board colid = 
  let row = ref "" in
  for i = 1 to 8 do
    row := !row ^ (Board.get_piece (colid ^ string_of_int i) board)
  done; 
  !row

let init_pos_test name board colid expected = 
  let col = make_col board colid in 
  name >:: fun _ -> assert_equal (col) expected

let board = init () 
let init_tests = [
  init_pos_test "First col is RPNANANANApr" board "a" "RPNANANANApr";
  init_pos_test "Second col is NPNANANANApn" board "b" "NPNANANANApn";
  init_pos_test "Third col is BPNANANANApb" board "c" "BPNANANANApb" ;
  init_pos_test "Fourth col is QPNANANANApq" board "d" "QPNANANANApq";
  init_pos_test "Fifth col is KPNANANANApk" board "e" "KPNANANANApk";
]
(**[move_throws board from_sqr to_sqr expected] creates an OUnit test
   asserting that movement of a piece at [from_sqr] to [to_sqr] throws
   [IllegalMove of error]*)
let move_throws name board move_str error =
  let pos = fen_to_board board in
  name >:: fun _ ->
  assert_raises (IllegalMove error) (fun () -> move move_str "" pos)

(*FENs for knight tests*)
let knight_tests = []

(*FENs for bishop tests*)
let bishop_tests = []

(*FENs for rook tests*)
let rook_tests = []

(*FENs for queen tests*)
let queen_tests = []

(*FENs for pawn tests*)
let pawn_tests = []

let check_true name fen move_str =
  let pos = fen_to_board fen in
  let next_pos = move move_str "" pos in
  name >:: fun _ -> assert_equal true (is_in_check next_pos)

(**bishop FENs and movestrings for is_check tests*)
let b1 = "rnbqkbnr/pp2p1pp/8/2pP4/3P4/8/PPP1BPPP/RNBQK1NR w KQkq - 0 3"

let b1move = "e2h5"

let b1move2 = "e2b5"

let b2 =
  "rnbqkbnr/pp4pp/4p3/3P4/2p5/3P1P2/PPP1B1PP/RNBQ1RK1 b Qkq - 0 3"

let b2move = "f8c5"

let b3 =
  "rnbqkbnr/p5pp/1p2p3/2p5/3P4/3P2P1/PPP1BP1P/RNBQ1R1K b Qkq - 0 3"

let b3move = "c8b7"

let b4 =
  "rn1qkbnr/p2b2pp/1p6/2pK4/3P4/3P2P1/PPP1BP1P/RNBQ1R2 b kq - 0 3"

let b4move = "d7c6"

let b4move2 = "d7e6"

let b5 = "rn2kbnr/p5pp/1p6/2pK4/3P4/3b2P1/PPP1BP1P/RNBQ1R2 b kq - 0 3"

let b5move = "d3e4"

let b5move2 = "d3c4"

let bishop_check_tests =
  [
    check_true "bottom right white bishop checks successfully" b1 b1move;
    check_true "bottom left white bishop checks successfully" b1 b1move2;
    check_true "top left black bishop checks successfully" b2 b2move;
    check_true "top left black bishop light squares checks successfully"
      b3 b3move;
    check_true
      "top left black bishop one square away checks successfully" b4
      b4move;
    check_true
      "top right black bishop one square away checks successfully" b4
      b4move2;
    check_true
      "bottom right black bishop one square away checks successfully" b5
      b5move;
    check_true
      "bottom left black bishop one square away checks successfully" b5
      b5move2;
  ]

(*knight FENs and movestrings for is_check tests*)
let right = "7K/8/8/2ppp3/2pkp2N/2ppp3/8/8 w - - 0 1"

let rmv1 = "h4f5"

let rmv2 = "h4f3"

let left = "7k/8/8/3PPP2/n2PKP2/3PPP2/8/8 b - - 0 1"

let lmv1 = "a4c5"

let lmv2 = "a4c3"

let top = "4n2k/8/8/3PPP2/3PKP2/3PPP2/8/8 b - - 0 1"

let tmv1 = "e8f6"

let tmv2 = "e8d6"

let bottom = "7K/8/2ppp3/2pkp3/2ppp3/8/8/3N4 w - - 0 1"

let bmv1 = "d1e3"

let bmv2 = "d1c3"

let knight_check_tests =
  [
    check_true "right top middle white knight checks black king" right
      rmv1;
    check_true "right bottom middle white knight checks black king"
      right rmv2;
    check_true "left top middle black knight checks white king" left
      lmv1;
    check_true "left bottom middle black knight checks white king" left
      lmv2;
    check_true "top right black knight checks white king" top tmv1;
    check_true "top left black knight checks white king" top tmv2;
    check_true "bottom right white knight checks black king" bottom bmv1;
    check_true "bottom left white knight checks black king" bottom bmv2;
  ]

(*pawn FENs and movestrings for is_check tests*)
let w1 = "rn1q1bnr/pp2pppp/2p5/5b2/1k1PN3/8/PPP2PPP/R1BQKBNR w KQ - 1 5"

let w1move = "c2c3"

let w2move = "a2a3"

let b1 = "rn1kqbnr/pp1ppppp/2p5/4Kb2/3PN3/8/PPP2PPP/R1BQ1BNR w kq - 1 5"

let b1move = "d7d6"

let b2move = "f7f6"

let dw =
  "rn1k1bnr/pp1ppppp/2p5/1k2qb2/3PN3/8/PPP2PPP/R1BQKNNR w KQ - 1 5"

let dwmove = "c2c4"

let pawn_check_tests =
  [
    check_true "right white pawn check works" w1 w1move;
    check_true "left white pawn check works" w1 w1move;
    check_true "right black pawn check works" w1 w1move;
    check_true "left black pawn check works" w1 w1move;
    check_true "double move white pawn check works" dw dwmove;
  ]

(*rook FENs and movestrings for is_check tests*)
let r1 = "5k2/3R4/6p1/7R/7p/7r/1r4P1/4K3 b - - 0 1"

let rmove1 = "b2b1"

let rmove2 = "h3h1"

let rmove3 = "b2e2"

let r2 = "5k2/3R4/6p1/7R/7p/7r/1r4P1/4K3 w - - 0 1"

let r2move1 = "h5h8"

let r2move2 = "d7d8"

let r2move3 = "d7f7"

let rook_check_tests =
  [
    check_true "left black rook checks correctly" r1 rmove1;
    check_true "left black rook checks correctly" r1 rmove2;
    check_true "top black rook checks correctly" r1 rmove3;
    check_true "right white rook checks correctly" r2 r2move1;
    check_true "left white rook checks correctly" r2 r2move2;
    check_true "bottom white rook checks correctly" r2 r2move3;
  ]

let check_tests =
  List.flatten
    [ bishop_check_tests; knight_check_tests; rook_check_tests ]

(**[move_no_throw asserts that a move completes successfully without
   throwing an error]*)
let move_no_throw name board move_str =
  let pos = fen_to_board board in
  name >:: fun _ ->
  assert_equal ()
    ( move move_str "" pos;
      () )

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
      qpin4 "f1d1";
    move_throws "bottom right white queen raises" qpin5 "d4e5"
      "Moving this piece would place you in check";
    move_no_throw
      "bottom right white queen can be captured without throwing" qpin5
      "d4f2";
    move_throws "bottom left black queen raises" qpin6 "d4e4"
      "Moving this piece would place you in check";
    move_throws "top right white queen raises" qpin7 "f3g5"
      "Moving this piece would place you in check";
    move_throws "top right white queen raises" qpin8 "d3d8"
      "Moving this piece would place you in check";
  ]

let move_tests = List.flatten [ pin_tests ]

let undo_move_tests = []

let board_tests =
  List.flatten [ init_tests; move_tests; undo_move_tests; check_tests ]

let suite =
  "test suite for chess engine & game" >::: List.flatten [ board_tests ]

let _ = run_test_tt_main suite
