import chess/board
import chess/color.{White}
import chess/coord
import chess/piece.{Bishop, King, Knight, Pawn, Queen, Rook}
import gleam/list
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
    |> list.map(piece.piece_to_value)
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
    |> list.map(piece.piece_to_value)
    |> iv.from_list
    |> board.Board(8, 2, _)

  coord.new(3, 1)
  |> board.get_pos(board, _)
  |> should.equal(White |> King |> Ok)
}
