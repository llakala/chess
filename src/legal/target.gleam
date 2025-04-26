import chess/piece
import chess/position
import gleam/string

pub type Target {
  Target(pos: position.Position, kind: MoveKind)
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

pub fn compare(first: Target, second: Target) {
  position.compare(first.pos, second.pos)
}

pub fn to_string(target: Target) -> String {
  let kind_str = case target.kind {
    Basic -> ""
    Capture -> "Capture"
    Passant -> "En Passant"
    QueenCastle -> "Castle Queenside"
    KingCastle -> "Castle Kingside"
    Promotion(piece) -> {
      let piece_name = piece.kind |> string.inspect
      "Promotion to " <> piece_name
    }
    PromotionCapture(piece) -> {
      let piece_name = piece.kind |> string.inspect
      " Promotion & Capture to " <> piece_name
    }
  }

  let pos_str = target.pos |> position.to_string

  pos_str <> " (" <> kind_str <> ")"
}
