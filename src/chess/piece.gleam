import chess/color.{type Color, Black, White}
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

pub fn to_value(piece: Piece) -> Int {
  // If white, adds 8 to the value
  case piece {
    None -> 0
    Pawn(color) -> 1 + color.to_value(color)
    Rook(color) -> 2 + color.to_value(color)
    Bishop(color) -> 3 + color.to_value(color)
    Knight(color) -> 4 + color.to_value(color)
    Queen(color) -> 5 + color.to_value(color)
    King(color) -> 6 + color.to_value(color)
  }
}

pub fn from_value(value: Int) -> Result(Piece, String) {
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

pub fn to_string(piece: Piece) -> String {
  case piece {
    None -> "Empty"
    Pawn(White) -> "♙"
    Pawn(Black) -> "♟"
    Rook(White) -> "♖"
    Rook(Black) -> "♜"
    Bishop(White) -> "♗"
    Bishop(Black) -> "♝"
    Knight(White) -> "♘"
    Knight(Black) -> "♞"
    Queen(White) -> "♕"
    Queen(Black) -> "♛"
    King(White) -> "♔"
    King(Black) -> "♚"
  }
}

/// Doesn't take the *entire* fen string: just the first part encoding the board
pub fn from_fen(char: String) {
  case char {
    "p" -> Pawn(Black) |> Ok
    "P" -> Pawn(White) |> Ok
    "n" -> Knight(Black) |> Ok
    "N" -> Knight(White) |> Ok
    "b" -> Bishop(Black) |> Ok
    "B" -> Bishop(White) |> Ok
    "r" -> Rook(Black) |> Ok
    "R" -> Rook(White) |> Ok
    "q" -> Queen(Black) |> Ok
    "Q" -> Queen(White) |> Ok
    "k" -> King(Black) |> Ok
    "K" -> King(White) |> Ok
    _ -> Error("Invalid character " <> char <> "for fen decoding!")
  }
}
