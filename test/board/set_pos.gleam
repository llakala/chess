import birdie
import chess/board
import chess/color.{White}
import chess/piece.{Pawn}
import chess/position.{Position}
import gleam/string

pub fn set_pos_passing_test() {
  let board = board.empty()
  let assert Ok(pos) = position.new(col: 7, row: 1)

  let assert Ok(board) = board.set_pos(board, pos, Pawn(White))

  board
  |> board.get_pos(pos)
  |> string.inspect
  |> birdie.snap(title: "Found a pawn!")
}

pub fn set_pos_failing_test() {
  let board = board.empty()

  // We don't use the `new` function as it checks for invalid input,
  // instead going around it to make an invalid input
  let pos = Position(8, 1)

  let error_result = board.set_pos(board, pos, Pawn(White))

  error_result
  |> string.inspect
  |> birdie.snap(title: "Couldn't set the piece due to invalid position!")
}
