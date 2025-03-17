import chess/board
import chess/color.{White}
import chess/piece.{Bishop, King, Knight, Pawn, Queen, Rook}
import chess/position
import gleam/list
import gleam/result
import gleeunit/should
import iv

const piece_list = [
  Pawn(White),
  Pawn(White),
  Pawn(White),
  Pawn(White),
  Pawn(White),
  Pawn(White),
  Pawn(White),
  Pawn(White),
  Rook(White),
  Knight(White),
  Bishop(White),
  King(White),
  Queen(White),
  Bishop(White),
  Knight(White),
  Rook(White),
]

pub fn to_string_test() {
  let board =
    piece_list
    |> list.map(piece.to_value)
    |> iv.from_list
    |> board.Board(8, 2, _)

  board.to_string(board)
  |> should.equal(
    "White Pawn, White Pawn, White Pawn, White Pawn, White Pawn, White Pawn, White Pawn, White Pawn\nWhite Rook, White Knight, White Bishop, White King, White Queen, White Bishop, White Knight, White Rook"
    |> Ok,
  )
}

pub fn get_pos_test() {
  let board =
    piece_list
    |> list.map(piece.to_value)
    |> iv.from_list
    |> board.Board(8, 2, _)

  use pos <- result.try(position.new(col: 3, row: 1))
  pos
  |> board.get_pos(board, _)
  |> should.equal(White |> King |> Ok)

  use pos <- result.try(position.new(3, 7))
  board.get_pos(board, pos)
  |> should.equal(Error(
    "Tried to access row index `7`, but the board only had `2` rows!",
  ))

  Ok(Nil)
}
