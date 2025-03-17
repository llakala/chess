// My own helper functions for working with `iv` arrays
import chess/array
import gleam/int

import chess/piece.{type Piece}
import chess/position.{type Position}

import gleam/bool
import gleam/list
import gleam/result
import gleam/string

import iv.{type Array}

// TODO: use a `new` function to make sure data can't be more than
// the number of valid rows and columns
pub type Board {
  Board(cols: Int, rows: Int, data: Array(Int))
}

/// Returns the piece at the given coordinate
/// Will return an error if the position is invalid
pub fn get_pos(board: Board, pos: Position) -> Result(Piece, String) {
  let row = pos.row
  let col = pos.col
  let rows = board.rows
  let cols = board.cols
  use <- bool.guard(
    row >= rows,
    Error(
      "Tried to access row index `"
      <> row |> int.to_string
      <> "`, but the board only had `"
      <> board.rows |> int.to_string
      <> "` rows!",
    ),
  )
  use <- bool.guard(
    col >= cols,
    Error(
      "Tried to access column index `"
      <> col |> int.to_string
      <> "`, but the board only had `"
      <> board.cols |> int.to_string
      <> "` columns!",
    ),
  )

  // 0 corresponds to top left of board
  let index: Int = position.get_index(pos)
  use piece_at_index: Int <- result.try(
    board.data |> iv.get(index) |> result.replace_error("Index invalid!"),
  )
  use piece_type: piece.Piece <- result.try(piece_at_index |> piece.from_value)

  piece_type |> Ok
}

pub fn to_string(board: Board) -> Result(String, String) {
  case
    board.data
    |> iv.try_map(piece.from_value)
  {
    Error(value) -> Error(value)
    Ok(value) ->
      value
      |> iv.map(piece.to_string)
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
