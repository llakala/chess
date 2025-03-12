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
  case len % times {
    0 -> {
      str
      |> string.split("")
      // `newliner` will leave it reversed. We reverse now before folding, so we don't just reverse the row order
      |> list.reverse
      |> list.fold([""], newliner)
      |> string.join("\n")
      |> Ok
    }
    _ -> Error("Number of splits doesn't go into the string's length")
  }
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
  |> result.unwrap_both
  // Definitely bad practice, but Result on a tostring feels gross. I'm sure I'll like it with more experience
}

/// Returns whether the coordinate is filled
/// Will return an error if the position is invalid
pub fn get_pos(board: BinBoard, pos: Coord) {
  case pos.row > board.rows || pos.col > board.cols {
    True -> Error("Invalid board position!")
    False -> {
      // 0 corresponds to top left of board
      let index = pos.row * board.cols + pos.col
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
  }
}

pub fn main() {
  let value = 9
  let my_board = BinBoard(2, 2, value)
  io.println("Board:")
  my_board |> to_string |> io.println

  io.println("")
  // let pos = Coord(1, 0)
  //
  // use var <- result.try(get_pos(my_board, pos))
  // io.debug(var)
  // Ok(var)
}
