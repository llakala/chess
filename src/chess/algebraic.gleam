import chess/board
import chess/game.{type Game}
import gleam/result
import piece/piece.{type Piece}
import piece/square
import position/change.{type Change}
import position/file
import position/move.{
  type Move, Capture, KingCastle, Promotion, PromotionCapture, QueenCastle,
}
import position/position

/// Creates a representation of the move in algebraic notation, for sending as our
/// HTTP response
pub fn notation(move: Move, game: Game) -> Result(String, String) {
  let square = board.get_pos(game.board, move.change.from)

  use piece <- result.try(
    square
    |> square.to_piece
    |> result.replace_error(
      "Move was from an empty square to somewhere else, which doesn't make sense!",
    ),
  )

  case move.kind {
    KingCastle -> "0-0" |> Ok
    QueenCastle -> "0-0-0" |> Ok

    Promotion(new_piece) -> {
      // Fails if the change was obviously invalid (if it was moving an empty
      // square somewhere else, for example)
      use change_str <- result.try(handle_generic(move.change, piece))

      // The algebraic notation for our desired piece (ex. `R`)
      // Fails if we're trying to promote into a Pawn, since pawns can't be
      // represented with a single-letter like the others. Thankfully, promoting
      // into a pawn is illegal!
      use desired_str <- result.try(new_piece |> piece.to_algebraic)

      Ok(change_str <> "=" <> desired_str)
    }

    PromotionCapture(new_piece) -> {
      let change_str = capture_pawn_logic(move.change)

      // The algebraic notation for our desired piece (ex. `R`)
      // Fails if we're trying to promote into a Pawn, since pawns can't be
      // represented with a single-letter like the others. Thankfully, promoting
      // into a pawn is illegal!
      use desired_str <- result.try(new_piece |> piece.to_algebraic)

      Ok(change_str <> "=" <> desired_str)
    }

    Capture if piece.kind == piece.Pawn -> capture_pawn_logic(move.change) |> Ok
    move.Passant -> capture_pawn_logic(move.change) |> Ok

    // Capturing doesn't need a special string unless you're a pawn, which is
    // already handled above. Very silly design, but I didn't create SAN!
    _ -> handle_generic(move.change, piece)
  }
}

/// Used for most Moves, which don't actually need special algebraic notation
/// handling. Returns an error if the change started from an empty square, since
/// that makes no sense - but other than that, accepts anything.
fn handle_generic(change: Change, piece: Piece) -> Result(String, String) {
  case piece.kind {
    // This is NOT for pawn captures - they're already handled
    piece.Pawn -> change.to |> position.to_string |> Ok

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

pub fn capture_pawn_logic(change: Change) {
  // "e", for example
  let file_str = change.from |> position.file_index |> file.to_string

  let destination_str = change.to |> position.to_string
  file_str <> "x" <> destination_str
}
