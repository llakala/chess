import chess/piece.{type Piece}
import chess/position.{type Position}

pub type Square {
  Square(piece: Piece, position: Position)
}

pub fn to_piece(square: Square) -> Piece {
  square.piece
}

pub fn to_position(square: Square) -> Position {
  square.position
}
