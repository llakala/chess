import gleam/order.{type Order}
import gleam/string
import piece/piece
import position/change.{type Change}

pub type Move {
  Move(change: Change, kind: MoveKind)
}

pub type MoveKind {
  Basic
  Capture
  Promotion(new: piece.Piece)
  PromotionCapture(new: piece.Piece)
  Passant
  QueenCastle
  KingCastle
}

pub fn compare(first: Move, second: Move) -> Order {
  change.compare(first.change, second.change)
}

pub fn to_string(move: Move) -> String {
  kind_to_string(move.kind) <> change.to_string(move.change)
}

pub fn kind_to_string(kind: MoveKind) {
  case kind {
    Basic -> ""
    Capture -> "Capture "
    Passant -> "En Passant "
    QueenCastle -> "Castle Queenside "
    KingCastle -> "Castle Kingside "
    Promotion(piece) -> {
      let piece_name = piece.kind |> string.inspect
      piece_name <> " Promotion "
    }
    PromotionCapture(piece) -> {
      let piece_name = piece.kind |> string.inspect
      piece_name <> " Promotion & Capture "
    }
  }
}
