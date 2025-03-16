import chess/array
import chess/coord.{type Coord}
import chess/piece
import gleam/bool
import gleam/io
import gleam/result
import iv.{type Array}

// TODO: use a `new` function to make sure data can't be more than
// the number of valid rows and columns
pub type BinBoard {
  BinBoard(cols: Int, rows: Int, data: Array(Int))
}

pub fn main() {
  let values: List(Int) = [0, 1, 2, 3]
  let board: Result(String, String) =
    values |> iv.from_list |> BinBoard(2, 2, _) |> to_string

  io.println("Board:")
  case board {
    Ok(value) -> value |> io.println
    Error(value) -> { "Error: " <> value } |> io.println
  }
  // io.println("")
  // let pos = coord.new(1, 1)
  // coord.to_string(pos) |> io.pri|ntln
  //
  // case get_pos(my_board, pos) {
  //   Ok(val) -> val |> bool.to_string |> io.println
  //   Error(val) -> io.println("Error: " <> val)
  // }
}

pub fn to_string(board: BinBoard) -> Result(String, String) {
  case
    board.data
    |> iv.try_map(piece.value_to_piece)
  {
    Error(value) -> Error(value)
    Ok(value) ->
      value
      |> iv.map(piece.piece_to_string)
      |> format_list(board.rows)
  }
}

/// Break a list of strings into N sections, separated on newlines.
/// Returns an error if string wasn't divisible by number of times
fn format_list(lst: Array(String), times: Int) -> Result(String, String) {
  use chunked: Array(Array(String)) <- result.try(array.sized_chunk(lst, times))

  chunked
  |> iv.map(array.join(_, ", "))
  |> array.join("\n")
  |> Ok
}

/// Returns whether the coordinate is filled
/// Will return an error if the position is invalid
pub fn get_pos(board: BinBoard, pos: Coord) -> Result(Bool, String) {
  use <- bool.guard(
    pos.row >= board.rows || pos.col >= board.cols,
    Error("Invalid board position!"),
  )

  // 0 corresponds to top left of board
  let index: Int = pos.index
  use piece_at_index: Int <- result.try(
    board.data |> iv.get(index) |> result.replace_error("Index invalid!"),
  )
  use piece_type: piece.Piece <- result.try(
    piece_at_index |> piece.value_to_piece,
  )

  { piece_type != piece.None } |> Ok
}
