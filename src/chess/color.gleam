pub type Color {
  White
  Black
}

pub fn from_fen(fen: String) -> Result(Color, String) {
  case fen {
    "w" -> Ok(White)
    "b" -> Ok(Black)
    _ ->
      Error(
        "Invalid fen string for color! Expected `w` or `b`, but got `"
        <> fen
        <> "`.",
      )
  }
}

/// Given some color, return the other color. Great for finding what your enemy
/// would do!
pub fn invert(color: Color) -> Color {
  case color {
    White -> Black
    Black -> White
  }
}
