import legal/targets.{type Target}
import position/position.{type Position}

pub type Tarmap {
  Tarmap(origin: Position, targets: List(Target))
}
