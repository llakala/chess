import birdie
import chess/board
import gleam/string
import piece/color.{White}
import piece/piece.{Pawn, Piece}
import piece/square.{Some}
import position/position

pub fn set_pos_passing_test() {
  let board = board.empty()
  let assert Ok(pos) = position.from_indices(col: 7, row: 1)

  let square = Piece(Pawn, White) |> Some

  board
  |> board.set_pos(pos, square)
  |> board.get_pos(pos)
  |> string.inspect
  |> birdie.snap(title: "Found a pawn!")
}
