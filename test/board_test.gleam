import chess/board
import chess/choose
import chess/color.{Black, White}
import chess/piece.{Bishop, King, Knight, None, Pawn, Queen, Rook}
import chess/position
import gleam/io
import gleam/result
import gleam/string
import gleeunit/should
import iv

const full = [
  Rook(Black),
  Knight(Black),
  Bishop(Black),
  Queen(Black),
  King(Black),
  Bishop(Black),
  Knight(Black),
  Rook(Black),
  Pawn(Black),
  Pawn(Black),
  Pawn(Black),
  Pawn(Black),
  Pawn(Black),
  Pawn(Black),
  Pawn(Black),
  Pawn(Black),
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
  None,
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
  Queen(White),
  King(White),
  Bishop(White),
  Knight(White),
  Rook(White),
]

pub fn to_string_test() {
  let empty_board = board.empty()

  // Gleeunit doesn't seem to check that result.try always passes, which I
  // expected it to. We make a temp variable and assert that the result is
  // Ok to simulate tehis
  let board_result = iv.initialise(64, fn(_) { Pawn(White) }) |> board.create
  board_result |> should.be_ok

  use full_board <- result.try(board_result)

  board.to_string(empty_board)
  |> should.equal(
    "0, 0, 0, 0, 0, 0, 0, 0\n0, 0, 0, 0, 0, 0, 0, 0\n0, 0, 0, 0, 0, 0, 0, 0\n0, 0, 0, 0, 0, 0, 0, 0\n0, 0, 0, 0, 0, 0, 0, 0\n0, 0, 0, 0, 0, 0, 0, 0\n0, 0, 0, 0, 0, 0, 0, 0\n0, 0, 0, 0, 0, 0, 0, 0"
    |> Ok,
  )

  board.to_string(full_board)
  |> should.equal(
    "♙, ♙, ♙, ♙, ♙, ♙, ♙, ♙\n♙, ♙, ♙, ♙, ♙, ♙, ♙, ♙\n♙, ♙, ♙, ♙, ♙, ♙, ♙, ♙\n♙, ♙, ♙, ♙, ♙, ♙, ♙, ♙\n♙, ♙, ♙, ♙, ♙, ♙, ♙, ♙\n♙, ♙, ♙, ♙, ♙, ♙, ♙, ♙\n♙, ♙, ♙, ♙, ♙, ♙, ♙, ♙\n♙, ♙, ♙, ♙, ♙, ♙, ♙, ♙"
    |> Ok,
  )

  Ok(Nil)
}

pub fn get_pos_test() {
  let empty_board = board.empty()

  use pos <- result.try(position.new(col: 3, row: 1))
  pos
  |> board.get_pos(empty_board, _)
  |> should.equal(piece.None |> Ok)

  use pos <- result.try(position.new(3, 64))
  board.get_pos(empty_board, pos)
  |> should.be_error

  Ok(Nil)
}

pub fn from_fen_test() {
  let board_result: Result(board.Board, String) =
    board.from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")

  use board <- choose.cases(board_result, on_error: fn(err_value) {
    io.println("Failed to convert from fen: \n" <> err_value)
    should.fail()
  })

  use string <- choose.cases(board.to_string(board), on_error: fn(err_value) {
    io.println("to_string failed: \n" <> err_value)
    should.fail()
  })

  // Note that this only prints if the test fails! Little unintuitive from gleeunit, but whatever
  string
  |> string.append("\n", _)
  |> io.println

  board_result
  |> result.map(fn(board) { board.data |> iv.to_list })
  |> should.equal(Ok(full))
}
