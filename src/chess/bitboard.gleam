import gleam/bit_array
import gleam/io
import gleam/list
import gleam/result

type BitBoard {
  BitBoard(width: Int, height: Int, data: BitArray)
}

// Create an empty bit array
fn new(width: Int, height: Int) -> BitBoard {
  list.repeat(<<0:size(width)>>, height)
  |> list.fold(<<>>, bit_array.append)
  |> BitBoard(width, height, _)
}

// Equivalent of `list.each for a bitboard`
fn each(board: BitBoard, func: fn(BitArray) -> a) -> Nil {
  let width = board.width
  let height = board.height
  case board.data {
    <<>> -> Nil
    _ -> {
      let first =
        board.data
        |> bit_array.slice(0, 1)
        |> result.unwrap(<<255>>)

      let rest =
        board.data
        |> bit_array.slice(1, height - 1)
        |> result.unwrap(<<255>>)
        |> BitBoard(width, height - 1, _)

      func(first)
      each(rest, func)
    }
  }
}

// fn display(board: BitBoard) {
//   let height = board.height
//   let width = board.width
//   board.data |> bit_array.fold
// }

pub fn main() {
  let output = new(8, 8)

  // Even though it's a string, we can't use io.println
  // output.data |> bit_array.inspect |> io.debug
  output |> each(io.debug)
  // Concatenation
  // let first = <<4>>
  // let second = <<2>>
  // io.debug(<<first:bits, second:bits>>)
}
