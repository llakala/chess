//// Represents all the potential moves we could make from one position on the
//// board.

import chess/board
import chess/game.{type Game}
import chess/position.{type Position}
import chess/square
import gleam/list
import gleam/string
import legal/target.{type Target}
import utils/text

pub type MovesFromPosition {
  MovesFromPosition(origin: Position, targets: List(Target))
}

/// Sorts all the targets
pub fn sort(moves: MovesFromPosition) {
  let sorted = list.sort(moves.targets, target.compare)
  MovesFromPosition(moves.origin, sorted)
}

pub fn to_string(moves: MovesFromPosition) {
  let origin_str = moves.origin |> position.to_string

  let targets_str =
    moves.targets
    |> list.map(fn(target) { target |> target.to_string })
    |> string.inspect

  origin_str <> " -> " <> targets_str
}

/// Given some moves from a position, format/sort the targets, and show the origin
/// and destinations on the board.
pub fn display(moves: MovesFromPosition, game: Game) -> String {
  let origin = moves.origin
  let destinations = moves.targets |> list.map(fn(target) { target.pos })

  // Takes a square and an index, and gives it a color if it's the origin or one
  // of the destinations
  let colorize_square = fn(square, index) {
    let assert Ok(pos) = index |> position.from_data_index
    let square_str = square |> square.to_string

    case pos == origin {
      True -> square_str |> text.color(text.Cyan)

      False ->
        // If the current square's position is one of our destinations
        case list.contains(destinations, pos) {
          False -> square_str

          True -> square_str |> text.color(text.Yellow)
        }
    }
  }

  let moves_output =
    moves
    |> sort
    |> to_string

  let board_output = board.index_format(game.board, colorize_square)

  moves_output <> "\nBoard:\n" <> board_output
}
