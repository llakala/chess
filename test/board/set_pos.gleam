import birdie
import chess/board
import chess/color.{White}
import chess/piece.{Pawn}
import chess/position
import gleam/string

pub fn set_pos_passing_test() {
  let board = board.empty()
  let assert Ok(pos) = position.from_index(col: 7, row: 1)

  let assert Ok(board) = board.set_pos(board, pos, Pawn(White))

  board
  |> board.get_pos(pos)
  |> string.inspect
  |> birdie.snap(title: "Found a pawn!")
}
