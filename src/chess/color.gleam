pub type Color {
  White
  Black
}

pub fn color_to_value(color: Color) {
  case color {
    White -> 0
    Black -> 8
  }
}

pub fn color_to_string(color: Color) {
  case color {
    White -> "White"
    Black -> "Black"
  }
}
