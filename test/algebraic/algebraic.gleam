import birdie
import chess/algebraic
import chess/change
import chess/color.{Black}
import chess/game
import chess/move.{Move, PromotionCapture}
import chess/piece.{Bishop, Piece, Queen}
import gleeunit/should

pub fn promotion_test() {
  // Empty board, other than a black pawn on f2
  let assert Ok(game) = game.new("8/8/8/8/8/8/5p2/8 b KQkq - 0 1")

  let assert Ok(change) = change.new("f2", "f1")
  let piece = Piece(Queen, Black)

  let assert Ok(output) =
    Move(change, move.Promotion(piece))
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
    Move(change, PromotionCapture(piece))
    |> algebraic.notation(game)

  output
  |> birdie.snap(
    "Black pawn on g2 capturing to h1 and promoting to a bishop is represented algebraically as `gxh1=B`!",
  )
}

pub fn passant_algebraic_test() {
  let assert Ok(game) =
    // Random game with a legal en passant
    game.new("rnb2bn1/pp1p4/2p5/4k3/3p2Pp/5K2/P6N/8 b - g3 0 26")

  let assert Ok(change) = change.new("h4", "g3")
  let move = move.Move(change, move.Passant)

  let assert Ok(alg) = move |> algebraic.notation(game)
  alg |> should.equal("hxg3")
}
