import birdie
import chess/board
import chess/position
import gleam/string

pub fn white_pawn_test() {
  let board = board.initial()
  let assert Ok(pos) = position.new("e2")

  board.get_pos(board, pos)
  |> string.inspect
  |> birdie.snap(title: "Found a white pawn!")
}

pub fn white_queen_test() {
  let board = board.initial()
  let assert Ok(pos) = position.new("d1")

  board.get_pos(board, pos)
  |> string.inspect
  |> birdie.snap(title: "Found a white queen!")
}
