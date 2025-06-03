import gleam/int
import gleam/order.{type Order}
import piece/color
import piece/piece.{type Piece}
import position/move.{
  type Move, type MoveKind, Basic, Capture, KingCastle, Passant, Promotion,
  PromotionCapture, QueenCastle,
}

/// Returns the value of some piece. Value is positive for white pieces, and
/// negative for black pieces.
pub fn piece(piece: Piece) -> Int {
  let value = piece_value(piece)

  let sign = case piece.color {
    color.White -> 1
    color.Black -> -1
  }

  sign * value
}

fn piece_value(piece: Piece) -> Int {
  case piece.kind {
    piece.Pawn -> 1
    piece.Knight -> 3
    piece.Bishop -> 3

    piece.Queen -> 9
    piece.Rook -> 5

    // I should probably eventually give this a value, to de-incentivize check so
    // we end up not even needing check logic for move application - but not
    // bothering with it for now.
    piece.King -> 0
  }
}

/// Compare two moves based on their kind, to figure out how good they might be.
pub fn compare_moves(first: Move, second: Move) -> Order {
  let first_value = move(first.kind)
  let second_value = move(second.kind)

  // We compare backwards, so moves with high scores end up first
  int.compare(second_value, first_value)
}

/// Given some kind of move, return its value.
pub fn move(kind: MoveKind) {
  case kind {
    Basic -> 0
    QueenCastle | KingCastle -> 5
    Capture | Passant -> 10
    Promotion(piece) -> 20 + piece_value(piece)
    PromotionCapture(piece) -> 30 + piece_value(piece)
  }
}
