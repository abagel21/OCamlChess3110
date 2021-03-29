open OUnit2
open Board
open Piece

let init_tests = []

let move_tests = []

let undo_move_tests = []

let board_tests =
  List.flatten [ init_tests; move_tests; undo_move_tests ]

let suite = "test suite for chess engine & game" >::: List.flatten []

let _ = run_test_tt_main suite
