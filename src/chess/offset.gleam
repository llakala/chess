import chess/direction.{
  type Direction, Down, DownLeft, DownRight, Left, Right, Up, UpLeft, UpRight,
}

pub type Offset {
  Offset(vertical: Int, horizontal: Int)
}

/// Given a direction, and a distance in that direction, return an offset.
/// Note that offsets aren't checked to be valid - since they're not associated with
/// any position. The Position module is responsible for applying an offset to get
/// a new position, but even that won't check if the square is actually movable.
/// These limitations should be addressed by not working with offsets directly -
/// they're simply a helpful tool for modules like Generate.
pub fn in_direction(dir: Direction, dist: Int) -> Offset {
  let single_distance = directional_offsets(dir)

  Offset(single_distance.vertical * dist, single_distance.horizontal * dist)
}

pub fn knight_offsets() -> List(Offset) {
  [
    Offset(1, 2),
    Offset(1, -2),
    Offset(-1, 2),
    Offset(-1, -2),
    Offset(2, 1),
    Offset(2, -1),
    Offset(-2, 1),
    Offset(-2, -1),
  ]
}

fn directional_offsets(dir: Direction) -> Offset {
  case dir {
    Up -> Offset(1, 0)
    Down -> Offset(-1, 0)
    Right -> Offset(0, 1)
    Left -> Offset(0, -1)
    UpRight -> Offset(1, 1)
    DownLeft -> Offset(-1, -1)
    UpLeft -> Offset(1, -1)
    DownRight -> Offset(-1, 1)
  }
}
