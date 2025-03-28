import birdie
import chess/board
import chess/color.{White}
import chess/piece.{Pawn}
import chess/square.{Some}
import iv

pub fn to_string_empty_test() {
  let assert Ok(str) = board.to_string(board.empty())

  str |> birdie.snap(title: "Empty board")
}

pub fn to_string_full_board_test() {
  let square = Pawn(White) |> Some

  let assert Ok(board) = iv.initialise(64, fn(_) { square }) |> board.create

  let assert Ok(str) = board |> board.to_string

  str
  |> birdie.snap(title: "Board of only pawns")
}
