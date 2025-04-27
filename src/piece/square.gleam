//// A Square can be thought of as an Option(Piece). Since a Piece can't be set to
//// None, but a board needs to represent empty space, we instead make the board
//// a list of Squares. If your function can work with an empty piece, it should
//// take a Square. If your function actually expects a piece, it should take a
//// Piece.

import piece/piece.{type Piece}

pub type Square {
  Some(piece: Piece)
  None
}

pub fn new(piece: Piece) -> Square {
  Some(piece)
}

pub fn to_piece(square: Square) -> Result(Piece, String) {
  case square {
    Some(piece) -> Ok(piece)
    None -> Error("Can't represent None as a piece!")
  }
}

pub fn to_string(square: Square) -> String {
  case square {
    None -> "0"

    Some(piece) -> piece.to_icon(piece)
  }
}
