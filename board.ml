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
(**RI= *)
type r = Piece array array


type t = {
  board : r;
  castling : bool array;
  ep : int tuple;
  turn: bool;
  prev : t
}

