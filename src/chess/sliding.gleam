import chess/color.{type Color}
import chess/constants
import chess/piece
import chess/square.{type Square}
import gleam/int
import gleam/result

pub type Direction {
  Up
  Down
  Left
  Right
  UpLeft
  UpRight
  DownLeft
  DownRight
}

/// For when you want a function to only take sliding pieces
pub type SlidingPiece {
  Rook(color: Color)
  Bishop(color: Color)
  Queen(color: Color)
  King(color: Color)
}

/// QOL function that chains a `Square -> Piece -> Sliding Piece` conversion
pub fn from_square(square: Square) -> Result(SlidingPiece, String) {
  use piece <- result.try(square |> square.to_piece)
  piece |> from_piece
}

/// Take a generic piece and turn it into a Sliding piece. Return an error if
/// the piece wasn't a sliding piece.
pub fn from_piece(piece: piece.Piece) -> Result(SlidingPiece, String) {
  let color = piece.color
  case piece {
    piece.Knight(_) -> Error("Knights aren't sliding pieces!")
    piece.Pawn(_) -> Error("Pawns aren't sliding pieces!")

    piece.Bishop(_) -> Bishop(color) |> Ok
    piece.King(_) -> King(color) |> Ok
    piece.Queen(_) -> Queen(color) |> Ok
    piece.Rook(_) -> Rook(color) |> Ok
  }
}

pub fn to_piece(sliding_piece: SlidingPiece) -> piece.Piece {
  let color = sliding_piece.color
  case sliding_piece {
    Rook(_) -> piece.Rook(color)
    Bishop(_) -> piece.Bishop(color)
    Queen(_) -> piece.Queen(color)
    King(_) -> piece.King(color)
  }
}

/// QOL function that calls the `piece.to_string` so you don't have to manually
/// cast.
pub fn to_string(piece: SlidingPiece) -> String {
  piece |> to_piece |> piece.to_string
}

/// Return the maximum distance that a given piece can go
pub fn piece_distance(piece: SlidingPiece, dir: Direction) -> Int {
  case piece {
    // That was easy!
    King(_) -> 1

    Rook(_) -> rook_distance(dir)
    Bishop(_) -> bishop_distance(dir)
    Queen(_) -> queen_distance(dir)
  }
}

fn rook_distance(dir: Direction) -> Int {
  // Subtract 1 since we're currently on a piece
  let horizontal_distance = constants.row_len - 1
  let vertical_distance = constants.col_len - 1

  case dir {
    Up | Down -> horizontal_distance
    Left | Right -> vertical_distance

    _ -> 0
  }
}

fn bishop_distance(dir: Direction) {
  // Subtract 1 since we're currently on a piece
  let horizontal_distance = constants.row_len - 1
  let vertical_distance = constants.col_len - 1

  case dir {
    UpLeft | UpRight | DownLeft | DownRight ->
      int.min(horizontal_distance, vertical_distance)

    _ -> 0
  }
}

fn queen_distance(dir: Direction) -> Int {
  case dir {
    Up | Down | Left | Right -> rook_distance(dir)
    _ -> bishop_distance(dir)
  }
}
