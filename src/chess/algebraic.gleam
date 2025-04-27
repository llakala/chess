import chess/board
import chess/file
import chess/game.{type Game}
import chess/piece.{type Piece}
import chess/position.{type Position}
import chess/square
import gleam/result
import legal/target.{
  type MoveKind, Basic, Capture, KingCastle, Passant, Promotion,
  PromotionCapture, QueenCastle,
}

/// Creates a representation of the move in algebraic notation, for sending as our
/// HTTP response
pub fn notation(
  from from: Position,
  to to: Position,
  kind kind: MoveKind,
  game game: Game,
) -> Result(String, String) {
  case kind {
    KingCastle -> "0-0" |> Ok
    QueenCastle -> "0-0-0" |> Ok

    // Even though these are different records, they get the same logic for
    // generating the algebraic notation - since only pawns have special capture
    // notation, and we encoded that in the generic logic In the future, it might
    // make sense to move that logic here, and make the generic logic lighter.
    Promotion(new_piece) | PromotionCapture(new_piece) -> {
      // Fails if the change was obviously invalid (if it was moving an empty
      // square somewhere else, for example)
      use change_str <- result.try(generic_logic(from, to, game))

      // The algebraic notation for our desired piece (ex. `R`)
      // Fails if we're trying to promote into a Pawn, since pawns can't be
      // represented with a single-letter like the others.
      use desired_str <- result.try(new_piece |> piece.to_algebraic)

      Ok(change_str <> "=" <> desired_str)
    }

    // Capturing doesn't need a special string unless you're a pawn (handled in the
    // change function), and passant gets the same notation as a regular move. Very
    // silly design, but I didn't create SAN!
    Basic | Capture | Passant -> generic_logic(from, to, game)
  }
}

/// Works on any kind of move that doesn't need special handling
fn generic_logic(from: Position, to: Position, game: Game) {
  let square =
    game.board
    |> board.get_pos(from)

  use piece <- result.try(
    square
    |> square.to_piece
    |> result.replace_error(
      "Move was from an empty square to somewhere else, which doesn't make sense!",
    ),
  )
  case piece.kind {
    piece.Pawn -> pawn_logic(from, to, game, piece)
    _ -> {
      // We've already handled this, and I'm okay with assertions when the only error
      // would come from invalid logic
      let assert Ok(piece_str) = piece |> piece.to_algebraic

      // Competition uses the permissive chess.js parser - so it's okay for us to
      // always include both positions, even though it technically doesn't follow
      // Standard Algebraic Notation.
      let from_str = position.to_string(from)
      let to_str = position.to_string(to)
      Ok(piece_str <> from_str <> to_str)
    }
  }
}

fn pawn_logic(from: Position, to: Position, game: Game, piece: Piece) {
  let square = game.board |> board.get_pos(to)
  case square {
    square.None -> to |> position.to_string |> Ok

    // Invalid capture that tries to capture a friend
    square.Some(other_piece) if piece.color == other_piece.color ->
      Error(
        "Move was found to capture a piece of the same color, but that's illegal!",
      )

    // A valid capture of a piece of another color. In the future, this logic
    // may be split out to the Move module to handle, since it has records for
    // Capture.
    _ -> {
      // "e", for example
      let current_file_str = position.get_file(from) |> file.to_string

      let new_pos_str = position.to_string(to)

      Ok(current_file_str <> "x" <> new_pos_str)
    }
  }
}
