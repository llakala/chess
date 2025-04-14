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

/// Gives a unicode representation of the piece, for displaying in tests.
pub fn to_icon(piece: Piece) -> String {
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

/// Get the algebraic notation for a piece (a knight would get `N`, for example).
/// Returns an error if it recieves a pawn, since pawns need their own logic for
/// algebraic notation generation.
pub fn to_algebraic(piece: Piece) -> Result(String, String) {
  case piece {
    Knight(_) -> "N" |> Ok
    Bishop(_) -> "B" |> Ok
    King(_) -> "K" |> Ok
    Pawn(_) -> Error("Pawn algebraic notation can't be generated through this!")
    Queen(_) -> "Q" |> Ok
    Rook(_) -> "R" |> Ok
  }
}

/// Get the piece's name (i.e. "Queen"), for use in debugging in tests.
pub fn to_name(piece: Piece) {
  case piece {
    Bishop(_) -> "Bishop"
    King(_) -> "King"
    Knight(_) -> "Knight"
    Pawn(_) -> "Pawn"
    Queen(_) -> "Queen"
    Rook(_) -> "Rook"
  }
}
