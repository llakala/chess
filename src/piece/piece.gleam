import gleam/string
import piece/color.{type Color, Black, White}

/// A Piece stores what kind of piece it is, and the color of the piece.
/// Notably, a Piece cannot be None - if you want that, you should use the
/// Square type, which is essentially an optional piece.
pub type Piece {
  Piece(kind: PieceKind, color: Color)
}

/// This simply represents a specific type of piece (Pawn, Bishop, etc).. We
/// can't call it `PieceType`, because the word `type` is reserved, so
/// `piece.type` isn't allowed.
pub type PieceKind {
  Pawn
  Rook
  Bishop
  Knight
  Queen
  King
}

/// Doesn't take the *entire* fen string: just the first part encoding the board
pub fn from_fen(char: String) -> Result(Piece, String) {
  case char {
    "p" -> Piece(Pawn, Black) |> Ok
    "P" -> Piece(Pawn, White) |> Ok

    "n" -> Piece(Knight, Black) |> Ok
    "N" -> Piece(Knight, White) |> Ok

    "b" -> Piece(Bishop, Black) |> Ok
    "B" -> Piece(Bishop, White) |> Ok

    "r" -> Piece(Rook, Black) |> Ok
    "R" -> Piece(Rook, White) |> Ok

    "q" -> Piece(Queen, Black) |> Ok
    "Q" -> Piece(Queen, White) |> Ok

    "k" -> Piece(King, Black) |> Ok
    "K" -> Piece(King, White) |> Ok

    _ -> Error("Invalid character `" <> char <> "` for fen decoding!")
  }
}

/// Gives a unicode representation of the piece, for displaying in tests.
pub fn to_icon(piece: Piece) -> String {
  case piece.kind, piece.color {
    Pawn, White -> "♙"
    Pawn, Black -> "♟"

    Rook, White -> "♖"
    Rook, Black -> "♜"

    Bishop, White -> "♗"
    Bishop, Black -> "♝"

    Knight, White -> "♘"
    Knight, Black -> "♞"

    Queen, White -> "♕"
    Queen, Black -> "♛"

    King, White -> "♔"
    King, Black -> "♚"
  }
}

/// Get the algebraic notation for a piece (a knight would get `N`, for example).
/// Returns an error if it recieves a pawn, since pawns need their own logic for
/// algebraic notation generation.
pub fn to_algebraic(piece: Piece) -> Result(String, String) {
  case piece.kind {
    Knight -> "N" |> Ok
    Bishop -> "B" |> Ok
    King -> "K" |> Ok
    Pawn -> Error("Pawn algebraic notation can't be generated through this!")
    Queen -> "Q" |> Ok
    Rook -> "R" |> Ok
  }
}

/// Creates a string representation of a piece. For example, Piece(Knight,
/// Black) would return "Black Knight".
pub fn to_string(piece: Piece) -> String {
  let color = piece.color |> string.inspect
  let kind = piece.kind |> string.inspect

  color <> " " <> kind
}
