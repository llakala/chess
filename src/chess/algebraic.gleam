import chess/board
import chess/game.{type Game}
import gleam/result
import piece/piece.{type Piece}
import piece/square
import position/change.{type Change}
import position/file
import position/move.{
  type Move, KingCastle, Promotion, PromotionCapture, QueenCastle,
}
import position/position

/// Creates a representation of the move in algebraic notation, for sending as our
/// HTTP response
pub fn notation(move: Move, game: Game) -> Result(String, String) {
  case move.kind {
    KingCastle -> "0-0" |> Ok
    QueenCastle -> "0-0-0" |> Ok

    // Even though these are different records, they get the same logic for
    // generating the algebraic notation - since only pawns have special capture
    // notation, and we encoded that in the `handle_generic` function. In the
    // future, it might make sense to move that logic here.
    Promotion(new_piece) | PromotionCapture(new_piece) -> {
      // Fails if the change was obviously invalid (if it was moving an empty
      // square somewhere else, for example)
      use change_str <- result.try(handle_generic(move.change, game))

      // The algebraic notation for our desired piece (ex. `R`)
      // Fails if we're trying to promote into a Pawn, since pawns can't be
      // represented with a single-letter like the others.
      use desired_str <- result.try(new_piece |> piece.to_algebraic)

      Ok(change_str <> "=" <> desired_str)
    }

    // Capturing doesn't need a special string unless you're a pawn (handled in the
    // change function), and passant gets the same notation as a regular move. Very
    // silly design, but I didn't create SAN!
    _ -> handle_generic(move.change, game)
  }
}

/// Used for most Moves, which don't actually need special algebraic notation
/// handling. Returns an error if the change started from an empty square, since
/// that makes no sense - but other than that, accepts anything.
fn handle_generic(change: Change, game: Game) -> Result(String, String) {
  let square = board.get_pos(game.board, change.from)

  use piece <- result.try(
    square
    |> square.to_piece
    |> result.replace_error(
      "Move was from an empty square to somewhere else, which doesn't make sense!",
    ),
  )

  case piece.kind {
    piece.Pawn -> pawn_to_algebraic(game, change, piece)
    _ -> {
      // We've already handled this, and I'm okay with assertions when the only error
      // would come from invalid logic
      let assert Ok(piece_str) = piece |> piece.to_algebraic

      // Competition uses the permissive chess.js parser - so it's okay for us to
      // always include both positions, even though it technically doesn't follow
      // Standard Algebraic Notation.
      let old_pos_str = change.from |> position.to_string
      let new_pos_str = change.to |> position.to_string
      Ok(piece_str <> old_pos_str <> new_pos_str)
    }
  }
}

fn pawn_to_algebraic(
  game: Game,
  change: Change,
  piece: Piece,
) -> Result(String, String) {
  let square = board.get_pos(game.board, change.to)
  case square {
    square.None -> change.to |> position.to_string |> Ok

    // Invalid capture that tries to capture a friend
    square.Some(other_piece) if piece.color == other_piece.color ->
      Error(
        "Move was found to capture a piece of the same color, but that's illegal!",
      )

    // A valid capture of a piece of another color.. In the future, this logic
    // may be split out to be handled only when the move is of type Capture.
    _ -> {
      // "e", for example
      let file_str = change.from |> position.get_file |> file.to_string

      let destination_str = change.to |> position.to_string
      Ok(file_str <> "x" <> destination_str)
    }
  }
}
