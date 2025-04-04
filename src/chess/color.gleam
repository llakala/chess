pub type Color {
  White
  Black
}

pub fn from_fen(fen: String) {
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
