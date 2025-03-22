import birdie
import chess/board
import chess/color.{White}
import chess/piece.{Pawn}
import chess/position
import gleam/string
import iv

pub fn to_string_empty_test() {
  let assert Ok(str) = board.to_string(board.empty())

  str |> birdie.snap(title: "Empty board")
}

pub fn to_string_full_board_test() {
  let assert Ok(board) =
    iv.initialise(64, fn(_) { Pawn(White) }) |> board.create

  let assert Ok(str) = board |> board.to_string

  str
  |> birdie.snap(title: "Board of only pawns")
}

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

pub fn from_fen_test() {
  let assert Ok(board) =
    board.from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")

  let assert Ok(str) =
    board
    |> board.to_string

  str |> birdie.snap(title: "The starting chess board arrangement")
}
