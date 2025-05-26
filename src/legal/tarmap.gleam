import gleam/list
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
