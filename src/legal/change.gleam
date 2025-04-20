import chess/board
import chess/file
import chess/game.{type Game, Game}
import chess/offset.{type Offset}
import chess/piece.{type Piece}
import chess/position.{type Position}
import chess/square
import gleam/order.{type Order}
import gleam/result

pub type Change {
  Change(from: Position, to: Position)
}

/// Simple constructor that takes strings and passes them to the position
/// constructor, so you don't have to manually create positions every single time.
pub fn new(from from_str: String, to to_str: String) -> Result(Change, String) {
  use from <- result.try(from_str |> position.new)
  use to <- result.try(to_str |> position.new)

  Change(from, to) |> Ok
}

pub fn compare(first: Change, second: Change) -> Order {
  // First try to sort on `from` - if you get `Eq`, then sort on `to`. Only if that
  // ALSO returns Eq do we return Eq.
  use <- order.lazy_break_tie(position.compare(first.from, second.from))
  position.compare(first.to, second.to)
}

/// Apply some change to the board directly, moving the value at one position to some
/// other position. Note that this is currently not checked to ensure that
/// `change.from` is non-empty - be careful!
pub fn apply(game: Game, change: Change) -> Game {
  let square = board.get_pos(game.board, change.from)
  let new_board =
    game.board
    |> board.set_pos(change.from, square.None)
    |> board.set_pos(change.to, square)

  // Update the game with the new board
  Game(..game, board: new_board)
}

/// Get the offset of the change - aka the distance the change takes it horizontally
/// and vertically.
pub fn to_offset(change: Change) -> Offset {
  let #(old_rank, old_file) = change.from |> position.to_indices
  let #(new_rank, new_file) = change.to |> position.to_indices

  let vertical_change = new_rank - old_rank
  let horizontal_change = new_file - old_file
  offset.Offset(vertical_change, horizontal_change)
}

pub fn to_string(change: Change) -> String {
  let from_str = change.from |> position.to_string
  let to_str = change.to |> position.to_string

  from_str <> " -> " <> to_str
}

/// Represent a given change in algebraic notation. Returns an error if the change
/// started from an empty square, since that makes no sense - but other than that,
/// accepts anything.
pub fn to_algebraic(change: Change, game: Game) -> Result(String, String) {
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
    // may be split out to the Move module to handle, since it has records for
    // Capture.
    _ -> {
      // "e", for example
      let current_file_str = change.from |> position.get_file |> file.to_string

      let new_pos_str = change.to |> position.to_string
      Ok(current_file_str <> "x" <> new_pos_str)
    }
  }
}
