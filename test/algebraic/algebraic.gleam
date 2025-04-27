import birdie
import chess/algebraic
import chess/game
import piece/color.{Black}
import piece/piece.{Bishop, Piece, Queen}
import position/change
import position/move

pub fn promotion_test() {
  // Empty board, other than a black pawn on f2
  let assert Ok(game) = game.new("8/8/8/8/8/8/5p2/8 b KQkq - 0 1")

  let assert Ok(change) = change.new("f2", "f1")
  let piece = Piece(Queen, Black)

  let assert Ok(output) =
    change
    |> move.Promotion(piece)
    |> algebraic.notation(game)

  output
  |> birdie.snap(
    "Black pawn on f2 moving to f1 and promoting to a queen is represented algebraically as `f1=Q`!",
  )
}

pub fn promotion_capture_test() {
  // Empty board, other than a black pawn on g2, and white kinght on h1
  let assert Ok(game) = game.new("8/8/8/8/8/8/6p1/7K b KQkq - 0 1")

  let assert Ok(change) = change.new("g2", "h1")
  let piece = Piece(Bishop, Black)

  let assert Ok(output) =
    change
    |> move.PromotionCapture(piece)
    |> algebraic.notation(game)

  output
  |> birdie.snap(
    "Black pawn on g2 capturing to h1 and promoting to a bishop is represented algebraically as `gxh1=B`!",
  )
}
// TODO: add castling tests
