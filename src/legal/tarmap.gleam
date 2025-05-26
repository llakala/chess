import chess/board
import chess/game
import gleam/list
import gleam/string
import legal/targets.{type Target}
import piece/square
import position/change
import position/move.{type Move}
import position/position.{type Position}
import utils/text

pub type Tarmap {
  Tarmap(origin: Position, targets: List(Target))
}

/// Turn a given Tarmap into a list of Moves from the origin position.
pub fn to_move(tarmap: Tarmap) -> List(Move) {
  let origin = tarmap.origin

  list.fold(tarmap.targets, [], fn(accum, target) {
    let change = change.Change(origin, target.destination)
    let move = move.Move(change, target.kind)

    [move, ..accum]
  })
}

pub fn to_string(tarmap: Tarmap) -> String {
  let origin_str = tarmap.origin |> position.to_string

  let targets_str =
    tarmap.targets |> list.map(targets.to_string) |> string.join(", ")

  origin_str <> " -> (" <> targets_str <> ")"
}

/// Compare two Tarmaps by their origin.
pub fn compare(first: Tarmap, second: Tarmap) {
  position.compare(first.origin, second.origin)
}

/// Display a list of tarmaps on some gameboard.
pub fn display(tarmaps: List(Tarmap), game: game.Game) {
  let origins = list.map(tarmaps, fn(tarmap) { tarmap.origin })
  let targets = list.flat_map(tarmaps, fn(tarmap) { tarmap.targets })
  let destinations = list.map(targets, fn(target) { target.destination })

  // Takes a square and an index, and colors in the origins and destinations of
  // the moves.
  let colorize_square = fn(square, index) {
    let assert Ok(pos) = index |> position.from_index
    let value = square.to_string
    let square_str = square |> value

    case list.contains(origins, pos), list.contains(destinations, pos) {
      // Neither an origin or a destination
      False, False -> square_str

      // Origin, not a destination
      True, False -> square_str |> text.color(text.Cyan)

      // Destination, not an origin
      False, True -> square_str |> text.color(text.Yellow)

      // Both an origin and a destination
      True, True -> square_str |> text.color(text.RGB)
    }
  }

  let tarmaps_output =
    tarmaps
    |> list.sort(compare)
    // Sort the list of targets for each tarmap
    |> list.map(fn(tarmap) {
      let Tarmap(origin, targets) = tarmap
      let targets = list.sort(targets, targets.compare)
      Tarmap(origin, targets)
    })
    |> list.map(to_string)
    |> string.inspect

  let board_output = board.index_format(game.board, colorize_square)

  tarmaps_output <> "\nBoard:\n" <> board_output
}
