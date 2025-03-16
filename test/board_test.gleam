import chess/board
import chess/color.{White}
import chess/coord
import chess/piece.{Bishop, King, Knight, Pawn, Queen, Rook}
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

  use pos <- result.try(coord.from_pair(3, 1))
  board.get_pos(board, pos)
  |> should.equal(White |> King |> Ok)

  use pos <- result.try(coord.from_pair(3, 7))
  board.get_pos(board, pos)
  |> should.equal(Error("Invalid board position!"))

  Ok(Nil)
}
