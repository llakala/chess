import chess/color.{type Color, Black, White, color_to_string, color_to_value}
import gleam/int

pub type Piece {
  None
  Pawn(color: Color)
  Rook(color: Color)
  Bishop(color: Color)
  Knight(color: Color)
  Queen(color: Color)
  King(color: Color)
}

pub fn piece_to_value(piece: Piece) -> Int {
  // If white, adds 8 to the value
  case piece {
    None -> 0
    Pawn(color) -> 1 + color_to_value(color)
    Rook(color) -> 2 + color_to_value(color)
    Bishop(color) -> 3 + color_to_value(color)
    Knight(color) -> 4 + color_to_value(color)
    Queen(color) -> 5 + color_to_value(color)
    King(color) -> 6 + color_to_value(color)
  }
}

pub fn piece_to_string(piece: Piece) -> String {
  case piece {
    None -> "Empty"
    Pawn(color) -> color_to_string(color) <> " Pawn"
    Rook(color) -> color_to_string(color) <> " Rook"
    Bishop(color) -> color_to_string(color) <> " Bishop"
    Knight(color) -> color_to_string(color) <> " Knight"
    Queen(color) -> color_to_string(color) <> " Queen"
    King(color) -> color_to_string(color) <> " King"
  }
}

pub fn value_to_piece(value: Int) -> Result(Piece, String) {
  case value {
    0 -> None |> Ok

    1 -> Pawn(Black) |> Ok
    2 -> Rook(Black) |> Ok
    3 -> Bishop(Black) |> Ok
    4 -> Knight(Black) |> Ok
    5 -> Queen(Black) |> Ok
    6 -> King(Black) |> Ok

    9 -> Pawn(White) |> Ok
    10 -> Rook(White) |> Ok
    11 -> Bishop(White) |> Ok
    12 -> Knight(White) |> Ok
    13 -> Queen(White) |> Ok
    14 -> King(White) |> Ok

    _ ->
      { "Invalid value " <> int.to_string(value) <> " encountered" }
      |> Error
  }
}
