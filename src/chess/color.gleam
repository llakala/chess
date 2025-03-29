pub type Color {
  White
  Black
}

pub fn to_value(color: Color) -> Int {
  case color {
    White -> 8
    Black -> 0
  }
}
