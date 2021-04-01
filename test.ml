open OUnit2
open Board
open Piece

let init_tests = []

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
  List.flatten [ init_tests; move_tests; undo_move_tests ]

let suite =
  "test suite for chess engine & game" >::: List.flatten [ board_tests ]

let _ = run_test_tt_main suite
