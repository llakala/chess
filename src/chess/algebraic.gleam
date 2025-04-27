import chess/game.{type Game}
import chess/piece
import gleam/result
import legal/change
import legal/move.{
  type Move, KingCastle, Promotion, PromotionCapture, QueenCastle,
}

/// Creates a representation of the move in algebraic notation, for sending as our
/// HTTP response
pub fn notation(move: Move, game: Game) -> Result(String, String) {
  case move {
    KingCastle(_) -> "0-0" |> Ok
    QueenCastle(_) -> "0-0-0" |> Ok

    // Even though these are different records, they get the same logic for
    // generating the algebraic notation - since only pawns have special capture
    // notation, and we encoded that in the `change.to_algebraic` function. In the
    // future, it might make sense to move that logic here, and make the `change`
    // module lighter.
    Promotion(_, new_piece) | PromotionCapture(_, new_piece) -> {
      // Fails if the change was obviously invalid (if it was moving an empty
      // square somewhere else, for example)
      use change_str <- result.try(change.to_algebraic(move.change, game))

      // The algebraic notation for our desired piece (ex. `R`)
      // Fails if we're trying to promote into a Pawn, since pawns can't be
      // represented with a single-letter like the others.
      use desired_str <- result.try(new_piece |> piece.to_algebraic)

      Ok(change_str <> "=" <> desired_str)
    }

    // Capturing doesn't need a special string unless you're a pawn (handled in the
    // change function), and passant gets the same notation as a regular move. Very
    // silly design, but I didn't create SAN!
    _ -> change.to_algebraic(move.change, game)
  }
}
