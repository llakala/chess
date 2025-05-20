import bot/eval
import position/move.{type Move}

/// Return the score of a given move
pub fn move(move: Move) -> Int {
  case move.kind {
    move.Basic -> 0
    move.KingCastle -> 0
    move.QueenCastle -> 0
    move.Passant -> 0

    move.Capture -> 10

    // Promotions are more valuable if you turn into a queen than a knight
    move.Promotion(piece) -> 10 + eval.piece_value(piece, piece.color)
    move.PromotionCapture(piece) -> 20 + eval.piece_value(piece, piece.color)
  }
}
