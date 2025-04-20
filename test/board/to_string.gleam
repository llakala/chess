import birdie
import chess/board
import chess/color.{White}
import chess/piece.{Pawn, Piece}
import chess/square.{Some}
import iv

pub fn to_string_empty_test() {
  board.empty() |> board.to_string |> birdie.snap(title: "Empty board")
}

pub fn to_string_full_board_test() {
  let square = Piece(Pawn, White) |> Some

  let assert Ok(board) = iv.initialise(64, fn(_) { square }) |> board.from_data

  board
  |> board.to_string
  |> birdie.snap(title: "Board of only pawns")
}
