import birdie
import chess/board

pub fn from_fen_test() {
  let board = board.initial()

  board
  |> board.to_string
  |> birdie.snap(title: "The starting chess board arrangement")
}
