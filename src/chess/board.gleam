// My own helper functions for working with `iv` arrays
import chess/array

import chess/coord.{type Coord}
import chess/piece.{type Piece, fen_to_piece, piece_to_value}

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

pub fn to_string(board: Board) -> Result(String, String) {
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

/// Returns the piece at the given coordinate
/// Will return an error if the position is invalid
pub fn get_pos(board: Board, pos: Coord) -> Result(Piece, String) {
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

  piece_type |> Ok
}
