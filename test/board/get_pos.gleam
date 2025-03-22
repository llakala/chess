import birdie
import chess/board
import chess/position
import gleam/string

pub fn get_pos_passing_test() {
  let board = board.empty()
  let assert Ok(pos) = position.new(col: 7, row: 1)

  let assert Ok(value_at_pos) = board.get_pos(board, pos)

  value_at_pos
  |> string.inspect
  |> birdie.snap(title: "Piece value of None!")
}

pub fn get_pos_failing_test() {
  // We don't use the `new` function as it checks for invalid input,
  // instead going around it to make an invalid input
  let pos = position.Position(col: 8, row: 1)

  // Expected to return an error
  board.get_pos(board.empty(), pos)
  |> string.inspect
  |> birdie.snap(title: "Failed to access position!")
}
