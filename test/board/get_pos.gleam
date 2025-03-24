import birdie
import chess/board
import chess/position
import gleam/string

pub fn get_pos_passing_test() {
  let board = board.initial()
  let assert Ok(pos) = position.from_algebraic("e2")

  let assert Ok(value_at_pos) = board.get_pos(board, pos)

  value_at_pos
  |> string.inspect
  |> birdie.snap(title: "Found a white pawn!")
}
