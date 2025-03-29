import chess/color.{type Color, Black, White}

pub type Piece {
  Pawn(color: Color)
  Rook(color: Color)
  Bishop(color: Color)
  Knight(color: Color)
  Queen(color: Color)
  King(color: Color)
}

/// Doesn't take the *entire* fen string: just the first part encoding the board
pub fn from_fen(char: String) -> Result(Piece, String) {
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

    _ -> Error("Invalid character `" <> char <> "` for fen decoding!")
  }
}

pub fn to_string(piece: Piece) -> String {
  case piece {
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
