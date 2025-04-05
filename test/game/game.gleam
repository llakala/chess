import birdie
import chess/game

pub fn starting_fen_test() {
  game.initial()
  |> game.to_string
  |> birdie.snap(
    "Initial fen string, with the starting board, White's turn, all directions castlable, no passant, zero halfmoves, and 1 fullmove",
  )
}
