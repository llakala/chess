import chess/piece
import gleam/order.{type Order}
import gleam/string
import legal/change.{type Change}

pub type Move {
  Basic(change: Change)
  Capture(change: Change)
  Promotion(change: Change, new: piece.Piece)
  PromotionCapture(change: Change, new: piece.Piece)
  Passant(change: Change)
  QueenCastle(change: Change)
  KingCastle(change: Change)
}

pub fn compare(first: Move, second: Move) -> Order {
  change.compare(first.change, second.change)
}

pub fn to_string(move: Move) -> String {
  let change_str = move.change |> change.to_string
  let move_str = case move {
    Basic(_) -> ""
    Capture(_) -> "Capture "
    Passant(_) -> "En Passant "
    QueenCastle(_) -> "Castle Queenside "
    KingCastle(_) -> "Castle Kingside "
    Promotion(_, piece) -> {
      let piece_name = piece.kind |> string.inspect
      piece_name <> " Promotion "
    }
    PromotionCapture(_, piece) -> {
      let piece_name = piece.kind |> string.inspect
      piece_name <> " Promotion & Capture "
    }
  }

  move_str <> change_str
}
