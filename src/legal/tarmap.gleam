import gleam/list
import gleam/string
import legal/targets.{type Target}
import position/change
import position/move.{type Move}
import position/position.{type Position}

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
