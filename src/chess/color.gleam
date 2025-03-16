pub type Color {
  White
  Black
}

pub fn to_value(color: Color) {
  case color {
    White -> 8
    Black -> 0
  }
}

pub fn to_string(color: Color) {
  case color {
    White -> "White"
    Black -> "Black"
  }
}
