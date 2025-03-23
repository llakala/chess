import birdie
import chess/board

pub fn from_fen_test() {
  let board = board.initial()

  let assert Ok(str) =
    board
    |> board.to_string

  str |> birdie.snap(title: "The starting chess board arrangement")
}
