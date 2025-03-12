import gleam/bool
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string

// TODO: use a `new` function to make sure data can't be more than
// the number of valid rows and columns
pub type BinBoard {
  BinBoard(cols: Int, rows: Int, data: Int)
}

pub type Coord {
  Coord(col: Int, row: Int)
}

/// Break a string into N sections, separated on newlines.
/// Returns an error if string wasn't divisible by number of times
fn break_string(str: String, times: Int) -> Result(String, String) {
  let len = string.length(str)

  // Pure function on a list of characters to add newlines every time we
  // reach the row's length
  let newliner = fn(acc: List(String), elem: String) -> List(String) {
    // Don't know why `let assert` is needed
    let assert [first, ..rest] = acc

    case string.length(first) >= len / times {
      True -> [elem, first, ..rest]
      False -> [elem <> first, ..rest]
    }
  }

  use <- bool.guard(
    len % times != 0,
    Error("Number of splits doesn't go into the string's length"),
  )

  str
  |> string.split("")
  // `newliner` will leave it reversed. We reverse now before folding, so we don't just reverse the row order
  |> list.reverse
  |> list.fold([""], newliner)
  |> string.join("\n")
  |> Ok
}

// Power function for integers, without needing to truncate
fn power(num: Int, power: Int) -> Int {
  let power = power |> int.to_float
  int.power(num, power)
  // Should be impossible, there's no way to get undefined for integers
  |> result.unwrap(0.0)
  |> float.truncate
}

pub fn to_string(board: BinBoard) -> String {
  board.data
  |> int.to_base2
  |> string.pad_start(board.cols * board.rows, "0")
  // Make the string the length of the board
  |> break_string(board.rows)
  // Definitely bad practice, but Result on a tostring feels gross. I'm sure I'll like it with more experience
  |> result.unwrap_both
}

/// Returns whether the coordinate is filled
/// Will return an error if the position is invalid
pub fn get_pos(board: BinBoard, pos: Coord) -> Result(Bool, String) {
  // We let the row and col have 1-based indexing for QOL
  // Be careful not to use the one from pos by accident!
  let col = pos.col - 1
  let row = pos.row - 1
  use <- bool.guard(
    row >= board.rows || col >= board.cols,
    Error("Invalid board position!"),
  )

  // 0 corresponds to top left of board
  let index = row * board.cols + col
  // Number of indices
  let positions = board.rows * board.cols

  // Subtract 1 so if index is 0, we get 2^{pos - 1}. For a 2x2,
  // This would be 2^3, which is what corresponds to the first
  // bit of a 4-bit binary number. From there, index just moves us
  // right by the correct number of bits
  let complement = power(2, positions - 1 - index)

  // We have an integer where the only bit that's on is the one
  // corresponding to the given index. We now bitwise and to
  // see if the bit is on in the data
  let anded = int.bitwise_and(board.data, complement)

  { anded > 0 } |> Ok
}

pub fn main() {
  let value = 0b000010001
  let my_board = BinBoard(3, 3, value)
  io.println("Board:")
  my_board |> to_string |> io.println

  io.println("")
  let pos = Coord(3, 3)

  case get_pos(my_board, pos) {
    Ok(val) -> val |> bool.to_string |> io.println
    Error(val) -> io.println("Error: " <> val)
  }
}
