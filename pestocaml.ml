(** PeSTOCaml adapts the PeSTO function found here at
    https://www.chessprogramming.org/PeSTO%27s_Evaluation_Function. *)
open Board

(* Piece Square Tables *)
let mg_pawn_table = [|
      0;   0;   0;   0;   0;   0;  0;   0; 98; 134;  61;  95;  68; 126; 34; -11;
     -6;   7;  26;  31;  65;  56; 25; -20; -14;  13;   6;  21;  23;  12; 17; 
     -23; -27;  -2;  -5;  12;  17;   6; 10; -25; -26;  -4;  -4; -10;   3;   3; 
     33; -12; -35;  -1; -20; -23; -15;  24; 38; -22; 0;   0;   0;   0;   0;   0;  
     0;   0;
|]
[@@ocamlformat "disable"]

let eg_pawn_table = [|
      0;   0;   0;   0;   0;   0;   0;   0; 178; 173; 158; 134; 147; 132; 165; 
      187; 94; 100;  85;  67;  56;  53;  82;  84; 32;  24;  13;   5;  -2;   4;  
      17;  17; 13;   9;  -3;  -7;  -7;  -8;   3;  -1; 4;   7;  -6;   1;   0;  
      -5;  -1;  -8; 13;   8;   8;  10;  13;   0;   2;  -7; 0;   0;   0;   0;   
      0;   0;   0;   0;
|]
[@@ocamlformat "disable"]

let mg_knight_table = [|
    -167; -89; -34; -49;  61; -97; -15; -107; -73; -41;  72;  36;  23;  62; 7;  
    -17; -47;  60;  37;  65;  84; 129;  73;   44; -9;  17;  19;  53;  37;  69;  
    18; 22; -13;   4;  16;  13;  28;  19;  21;   -8; -23;  -9;  12;  10;  19;  
    17;  25;  -16; -29; -53; -12;  -3;  -1;  18; -14;  -19; -105; -21; -58; -33; 
    -17; -28; -19;  -23;
|]
[@@ocamlformat "disable"]

let eg_knight_table = [|
    -58; -38; -13; -28; -31; -27; -63; -99; -25;  -8; -25;  -2;  -9; -25; -24; 
    -52; -24; -20;  10;   9;  -1;  -9; -19; -41; -17;   3;  22;  22;  22;  11;   
    8; -18; -18;  -6;  16;  25;  16;  17;   4; -18; -23;  -3;  -1;  15;  10; -3; 
    -20; -22; -42; -20; -10;  -5;  -2; -20; -23; -44; -29; -51; -23; -15; 
    -22; -18; -50; -64;
|]
[@@ocamlformat "disable"]

let mg_bishop_table = [|
    -29;   4; -82; -37; -25; -42;   7;  -8; -26;  16; -18; -13;  30;  59;  18; 
    -47; -16;  37;  43;  40;  35;  50;  37;  -2; -4;   5;  19;  50;  37;  37;   
    7;  -2; -6;  13;  13;  26;  34;  12;  10;   4; 0;  15;  15;  15;  14;  27;  
    18;  10; 4;  15;  16;   0;   7;  21;  33;   1; -33;  -3; -14; -21; -13; -12; 
    -39; -21;
|]
[@@ocamlformat "disable"]

let eg_bishop_table = [|
    -14; -21; -11; -8; -7; -9; -17; -24; -8;  -4; 7; -12; -3; -13;  -4; -14;
      2; -8; 0; -1; -2; 6; 0; 4; -3; 9; 12; 9; 14; 10; 3; 2; -6;   3;  13;  19;  
      7;  10;  -3;  -9; -12;  -3;   8;  10; 13;   3;  -7; -15; -14; -18;  -7;  
      -1;  4;  -9; -15; -27; -23;  -9; -23;  -5; -9; -16;  -5; -17;
|]
[@@ocamlformat "disable"]

let mg_rook_table = [|
     32;  42;  32;  51; 63;  9;  31;  43; 27;  32;  58;  62; 80; 67;  26;  44;
     -5;  19;  26;  36; 17; 45;  61;  16; -24; -11;   7;  26; 24; 35;  -8; -20;
    -36; -26; -12;  -1;  9; -7;   6; -23; -45; -25; -16; -17;  3;  0;  -5; -33;
    -44; -16; -20;  -9; -1; 11;  -6; -71; -19; -13;   1;  17; 16;  7; -37; -26;
|]
[@@ocamlformat "disable"]

let eg_rook_table = [|
    13; 10; 18; 15; 12;  12;   8;   5; 11; 13; 13; 11; -3;   3;   8;   3;
     7;  7;  7;  5;  4;  -3;  -5;  -3; 4;  3; 13;  1;  2;   1;  -1;   2;
     3;  5;  8;  4; -5;  -6;  -8; -11; -4;  0; -5; -1; -7; -12;  -8; -16;
    -6; -6;  0;  2; -9;  -9; -11;  -3; -9;  2;  3; -1; -5; -13;   4; -20;
|]
[@@ocamlformat "disable"]

let mg_queen_table = [|
    -28;   0;  29;  12;  59;  44;  43;  45; -24; -39;  -5;   1; -16;  57;  28;  
    54; -13; -17;   7;   8;  29;  56;  47;  57; -27; -27; -16; -16;  -1;  17;  
    -2;   1; -9; -26;  -9; -10;  -2;  -4;   3;  -3; -14;   2; -11;  -2;  -5;   
    2;  14;   5; -35;  -8;  11;   2;   8;  15;  -3;   1; -1; -18;  -9;  10; -15; 
    -25; -31; -50;
|]
[@@ocamlformat "disable"]

let eg_queen_table = [|
     -9;  22;  22;  27;  27;  19;  10;  20; -17;  20;  32;  41;  58;  25;  30;   
     0; -20;   6;   9;  49;  47;  35;  19;   9; 3;  22;  24;  45;  57;  40;  57;  
     36; -18;  28;  19;  47;  31;  34;  39;  23; -16; -27;  15;   6;   9;  17;  
     10;   5; -22; -23; -30; -16; -16; -23; -36; -32; -33; -28; -22; -43;  -5; 
     -32; -20; -41;
|]
[@@ocamlformat "disable"]

let mg_king_table = [|
    -65;  23;  16; -15; -56; -34;   2;  13; 29;  -1; -20;  -7;  -8;  -4; -38; 
    -29; -9;  24;   2; -16; -20;   6;  22; -22; -17; -20; -12; -27; -30; -25; 
    -14; -36; -49;  -1; -27; -39; -46; -44; -33; -51; -14; -14; -22; -46; -44; 
    -30; -15; -27; 1;   7;  -8; -64; -43; -16;   9;   8; -15;  36;  12; -54;   
    8; -28;  24;  14;
|]
[@@ocamlformat "disable"]

let eg_king_table = [|
    -74; -35; -18; -18; -11;  15;   4; -17; -12;  17;  14;  17;  17;  38;  23;  
    11; 10;  17;  23;  15;  20;  45;  44;  13; -8;  22;  24;  27;  26;  33;  26;
    3; -18;  -4;  21;  24;  27;  23;   9; -11; -19;  -3;  11;  21;  23;  16;   
    7;  -9; -27; -11;   4;  13;  14;   4;  -5; -17; -53; -34; -21; -11; -28; 
    -14; -24; -43
|]
[@@ocamlformat "disable"]

(** The table of piece-square tables during midgame. *)
let mg_pesto_table =
  [|
    mg_pawn_table;
    mg_knight_table;
    mg_bishop_table;
    mg_rook_table;
    mg_queen_table;
    mg_king_table;
  |]

(** The table of piece-square tables during endgame. *)
let eg_pesto_table =
  [|
    eg_pawn_table;
    eg_knight_table;
    eg_bishop_table;
    eg_rook_table;
    eg_queen_table;
    eg_king_table;
  |]

(** The empty tables for each phase. *)
let mg_table = Array.make_matrix 12 64 0

let eg_table = Array.make_matrix 12 64 0

(** The midgame values for each kind of piece. *)
let mg_value = [| 82; 337; 365; 477; 1025; 0 |]

(** The endgame values for each kind of piece. *)
let eg_value = [| 94; 281; 297; 512; 936; 0 |]

(** [int_of_piece p] returns the integer representation of a piece [p]:
    0 = pawn, 1 = knight, 2 = bishop, 3 = rook, 4 = queen, 5 = king, and
    12 = empty. White's pieces are [2 * n], and Black's are [2 * n + 1],
    where [n] is one of the aforementioned values. *)
let int_of_piece = function
  | "P" -> 0
  | "p" -> 1
  | "N" -> 2
  | "n" -> 3
  | "B" -> 4
  | "b" -> 5
  | "R" -> 6
  | "r" -> 7
  | "Q" -> 8
  | "q" -> 9
  | "K" -> 10
  | "k" -> 11
  | _ -> 12

(** [int_to_square n] returns [sq'], where [sq'] is the string
    representation of the [n]th index on an 8x8 chess board, i.e.
    [int_to_square_str 0] is ["a8"] and [int_to_square_str 63] is
    ["h1"]. Requires: [n] in 0 .. 63. *)
let int_to_square_str n =
  let rank, col = (n mod 8, (63 - n) / 8) in
  rank + 97 |> Char.chr |> Char.escaped |> fun x ->
  x ^ string_of_int (col + 1)

(** [flip n] returns the other player's equivalent position on the
    board, where [n] is the current index on the board. *)
let flip n = n lxor 56

(** [init_tables] populates the midgame and endgame tables for both
    Black and White pieces based on which squares they own on board
    [pos]. *)
let init_tables =
  let pc = ref 0 (* start at white pawn *) in
  (* goes through pawn, knight, bishop, rook, queen, in that order *)
  for p = 0 to 5 do
    for sq = 0 to 63 do
      mg_table.(!pc).(sq) <- mg_value.(p) + mg_pesto_table.(p).(sq);
      eg_table.(!pc).(sq) <- eg_value.(p) + eg_pesto_table.(p).(sq);
      mg_table.(!pc + 1).(sq) <-
        mg_value.(p) + mg_pesto_table.(p).(flip sq);
      eg_table.(!pc + 1).(sq) <-
        eg_value.(p) + eg_pesto_table.(p).(flip sq)
    done;
    pc := !pc + 2
  done

(** This tells us how to increment the game phase at each phase. *)
let game_phase_inc = [| 0; 0; 1; 1; 1; 1; 2; 2; 4; 4; 0; 0 |]

(** [eval_helper pos] loops through each piece on board [pos] and
    calculates the midgame and endgame scores for each player, as well
    as the current game_phase. *)
let eval_aux pos =
  let mg = [| 0; 0 |] in
  let eg = [| 0; 0 |] in
  let game_phase = ref 0 in
  (* eval_pieces evaluates each piece *)
  for sq = 0 to 63 do
    let sq_str = int_to_square_str sq in
    let pc = pos |> Board.get_piece sq_str |> int_of_piece in
    (* empty = 12 according to the original implementation *)
    if pc <> 12 then (
      let color = pc mod 2 (* 0 if white, 1 if black *) in
      (* update midgame points for both players and increment game phase *)
      mg.(color) <- mg.(color) + mg_table.(pc).(sq);
      eg.(color) <- eg.(color) + eg_table.(pc).(sq);
      game_phase := !game_phase + game_phase_inc.(pc))
  done;
  (mg, eg, game_phase)

let eval pos =
  let mg, eg, game_phase = eval_aux pos in
  let turn = Board.get_turn pos |> Bool.to_int in
  let mg_score = mg.(turn) - mg.(1 - turn) in
  let eg_score = eg.(turn) - eg.(1 - turn) in
  let mg_phase = !game_phase in
  let mg_phase' = if mg_phase > 24 then 24 else mg_phase in
  (* in case of early promotion *)
  let eg_phase = 24 - mg_phase' in
  ((mg_score * mg_phase) + (eg_score * eg_phase)) / 24
