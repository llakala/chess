import chess/constants
import gleam/int
import gleam/list
import gleam/result
import piece/color.{type Color}
import piece/piece.{type Piece, Piece}
import piece/square.{type Square}

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
  piece |> new
}

/// Take a generic piece and turn it into a Sliding piece. Return an error if
/// the piece wasn't a sliding piece.
pub fn new(piece: Piece) -> Result(SlidingPiece, String) {
  let color = piece.color
  case piece.kind {
    piece.Knight -> Error("Knights aren't sliding pieces!")
    piece.Pawn -> Error("Pawns aren't sliding pieces!")

    piece.Bishop -> Bishop(color) |> Ok
    piece.King -> King(color) |> Ok
    piece.Queen -> Queen(color) |> Ok
    piece.Rook -> Rook(color) |> Ok
  }
}

pub fn to_piece(sliding_piece: SlidingPiece) -> Piece {
  let color = sliding_piece.color
  case sliding_piece {
    Rook(_) -> Piece(piece.Rook, color)
    Bishop(_) -> Piece(piece.Rook, color)
    Queen(_) -> Piece(piece.Queen, color)
    King(_) -> Piece(piece.King, color)
  }
}

/// QOL function that calls the `piece.to_string` so you don't have to manually
/// cast.
pub fn to_string(piece: SlidingPiece) -> String {
  piece |> to_piece |> piece.to_icon
}

/// Get a list of all the directions a piece is able to go.
pub fn piece_directions(piece: SlidingPiece) -> List(Direction) {
  let straights = [Up, Down, Left, Right]
  let diagonals = [UpLeft, UpRight, DownLeft, DownRight]

  case piece {
    Rook(_) -> straights
    Bishop(_) -> diagonals
    Queen(_) -> list.append(straights, diagonals)
    King(_) -> list.append(straights, diagonals)
  }
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

fn bishop_distance(dir: Direction) -> Int {
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
