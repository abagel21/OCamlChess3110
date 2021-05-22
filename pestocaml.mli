(** PeSTOCaml adapts the PeSTO function (found here at
    https://www.chessprogramming.org/PeSTO%27s_Evaluation_Function) to
    OCaml. *)

(** [eval pos] evaluates a board [pos] and returns a value calculated
    via the PeSTO evaluation function. *)
val eval : Board.t -> int
