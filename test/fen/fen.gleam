import birdie
import chess/fen

pub fn starting_fen_test() {
  fen.initial()
  |> fen.to_string
  |> birdie.snap(
    "Initial fen string, with the starting board, White's turn, all directions castlable, no passant, zero halfmoves, and 1 fullmove",
  )
}
