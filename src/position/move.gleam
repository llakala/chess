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
  let change_str = move.change |> change.to_string
  let kind_str = case move.kind {
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

  kind_str <> change_str
}
