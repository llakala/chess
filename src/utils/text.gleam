// Goes at the end of a string to end the color
const reset = "\u{001b}[0m"

pub type Ansi {
  Yellow
  Cyan
  RGB
  Gray
}

fn to_ansi(color: Ansi) -> String {
  case color {
    Gray -> "\u{001B}[38;5;244m"
    Cyan -> "\u{001b}[36m"
    RGB -> "\u{001b}[38;2;"
    Yellow -> "\u{001b}[33m"
  }
}

pub fn color(text: String, color: Ansi) -> String {
  let ansi = to_ansi(color)
  ansi <> text <> reset
}
