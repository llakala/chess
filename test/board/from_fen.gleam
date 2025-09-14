import birdie
import chess/board
import chess/position
import gleam/string

pub fn from_fen_test() {
  let board = board.initial()

  board
  |> board.to_string
  |> birdie.snap(title: "The starting chess board arrangement")
}

pub fn not_inverted_test() {
  // Empty board, other than a pawn on e5
  let assert Ok(board) = board.new("8/8/8/4P3/8/8/8/8")
  let assert Ok(pos) = position.new("e5")

  let square = board |> board.get_pos(pos)

  square
  |> string.inspect
  |> birdie.snap("Should find a white pawn!")
}
