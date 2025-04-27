import birdie
import chess/board
import gleam/string
import position/position

pub fn get_pos_passing_test() {
  let board = board.initial()
  let assert Ok(pos) = position.new("e2")

  board.get_pos(board, pos)
  |> string.inspect
  |> birdie.snap(title: "Found a white pawn!")
}
