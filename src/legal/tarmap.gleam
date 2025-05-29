import chess/board
import chess/game
import gleam/dict
import gleam/list
import gleam/set
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

/// Type alias for convenience. When you have multiple tarmaps, it's preferred
/// to use this, rather than just a list. A dict prevents duplicates when it comes
/// to keys, and gives an easy way to merge multiple tarmap collections, which
/// is important for when we split things up during check filtering.
pub type TarmapCollection =
  dict.Dict(Position, List(Target))

/// Turn a given Tarmap into a list of Moves from the origin position.
pub fn tarmap_to_moves(tarmap: Tarmap) -> List(Move) {
  let origin = tarmap.origin

  list.fold(tarmap.targets, [], fn(accum, target) {
    let change = change.Change(origin, target.destination)
    let move = move.Move(change, target.kind)

    [move, ..accum]
  })
}

pub fn collection_to_moves(tarmaps: TarmapCollection) -> List(Move) {
  use accum, origin, targets <- dict.fold(tarmaps, [])
  let moves =
    list.fold(targets, [], fn(accum, target) {
      let change = change.Change(origin, target.destination)
      let move = move.Move(change, target.kind)

      [move, ..accum]
    })

  // Not great, but I don't want to change the moves data structure right now
  list.append(moves, accum)
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

pub fn from_list(tarmaps: List(Tarmap)) -> TarmapCollection {
  list.fold(tarmaps, dict.new(), fn(accum, tarmap) {
    let Tarmap(origin, targets) = tarmap
    dict.insert(accum, origin, targets)
  })
}

pub fn to_list(tarmaps: TarmapCollection) -> List(Tarmap) {
  tarmaps
  |> dict.to_list
  |> list.map(fn(tuple) { Tarmap(tuple.0, tuple.1) })
}

/// Display a collection of tarmaps on some gameboard.
pub fn display(tarmaps: TarmapCollection, game: game.Game) {
  let origins = dict.keys(tarmaps)

  // I wish we didn't have to flatten since we're linked lists, but this is
  // `display`, it's not performance-critical
  let targets = dict.values(tarmaps) |> list.flatten
  let destinations = targets.get_destinations(targets)

  // Takes a square and an index, and colors in the origins and destinations of
  // the moves.
  let colorize_square = fn(square, index) {
    let assert Ok(pos) = index |> position.from_index
    let value = square.to_string
    let square_str = square |> value

    case list.contains(origins, pos), set.contains(destinations, pos) {
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
    |> to_list
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

/// Given some collection of tarmaps, filter for the targets that satisfy some
/// condition. Works better than `list.filter`, since it lets you filter based
/// on individual origins and targets, and will remove tarmaps that end up
/// having no targets after filtering.
pub fn filter(
  tarmaps: TarmapCollection,
  func: fn(Position, Target) -> Bool,
) -> TarmapCollection {
  dict.fold(tarmaps, dict.new(), fn(accum, origin, targets) {
    let targets = list.filter(targets, fn(target) { func(origin, target) })

    case list.is_empty(targets) {
      // If filtering for True made one of the tarmaps point to no destinations,
      // don't include it
      True -> accum

      False -> dict.insert(accum, origin, targets)
    }
  })
}

/// Partition some list of tarmaps, based on a function taking an origin and a
/// target. This is more intelligent than `list.partition(tarmaps)` - it
/// lets you partition based on origins AND targets, so you can have all
/// the targets from some tarmap partioned into one side, or mix and match, with
/// some going to A, and some going to B.
pub fn partition(
  tarmaps: TarmapCollection,
  func: fn(Position, Target) -> Bool,
) -> #(TarmapCollection, TarmapCollection) {
  tarmaps
  // This makes each origin point to not just one List(Target), but instead two
  // lists of targets - the ones that returned True, and the ones that returned
  // False.
  |> dict.fold(dict.new(), fn(accum, origin, targets) {
    let #(first_targets, second_targets) =
      list.partition(targets, fn(target) { func(origin, target) })

    let tuple = #(first_targets, second_targets)

    dict.insert(accum, origin, tuple)
  })
  // We now want to have separate dicts - one for the True targets, one for the
  // False targets. We fold around two accums - the first dict, and the second
  // dict. This lets us move each element into the correct dict as we go!
  |> dict.fold(#(dict.new(), dict.new()), fn(accums, origin, both_targets) {
    let #(first_accum, second_accum) = accums
    let #(first_targets, second_targets) = both_targets

    // We don't want to bring a list of targets to the given accum if they're
    // empty after splitting - if that happens, we'll just not include that
    // origin in this accum.
    let first_accum = case list.is_empty(first_targets) {
      True -> first_accum
      False -> dict.insert(first_accum, origin, first_targets)
    }

    // See above!
    let second_accum = case list.is_empty(second_targets) {
      True -> second_accum
      False -> dict.insert(second_accum, origin, second_targets)
    }

    #(first_accum, second_accum)
  })
}
