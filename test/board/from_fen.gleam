import birdie
import chess/board

pub fn from_fen_test() {
  let assert Ok(board) =
    board.from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")

  let assert Ok(str) =
    board
    |> board.to_string

  str |> birdie.snap(title: "The starting chess board arrangement")
}
