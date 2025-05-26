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

/// Partition some list of tarmaps, based on a function taking an origin and a
/// target. This is more intelligent than `list.partition(tarmaps)` - it
/// lets you partition based on origins AND targets, so you can have all
/// the targets from some tarmap partioned into one side, or mix and match, with
/// some going to A, and some going to B.
pub fn partition(
  tarmaps: List(Tarmap),
  func: fn(Position, Target) -> Bool,
) -> #(List(Tarmap), List(Tarmap)) {
  tarmaps
  // This will transform us into a List(#(Tarmap, Tarmap)). But, we want a tuple
  // of lists!
  |> list.map(split_singular(_, func))
  // We fold around two lists - the first list, and the second list. This lets
  // us move each element into the correct partition as we go!
  |> list.fold(#([], []), fn(accums, elems) {
    let #(first_tarmap, second_tarmap) = elems

    let #(first_accum, second_accum) = accums

    // We don't want to bring a tarmap to the given accum if it has no
    // destinations after splitting, or we'll have a ton of junk origins
    // pointing to no destinations.
    let first_accum = case list.is_empty(first_tarmap.targets) {
      True -> first_accum
      False -> [first_tarmap, ..first_accum]
    }

    // See above!
    let second_accum = case list.is_empty(second_tarmap.targets) {
      True -> second_accum
      False -> [second_tarmap, ..second_accum]
    }

    #(first_accum, second_accum)
  })
}

/// Given some tarmap, split it into two separate tarmaps, based on some
/// function that acts on the given destination, and returns a Bool to decide
/// which tarmap the destination goes to.
fn split_singular(
  tarmap: Tarmap,
  func: fn(Position, Target) -> Bool,
) -> #(Tarmap, Tarmap) {
  let #(first_targets, second_targets) =
    list.partition(tarmap.targets, func(tarmap.origin, _))

  let first = Tarmap(tarmap.origin, first_targets)
  let second = Tarmap(tarmap.origin, second_targets)

  #(first, second)
}
