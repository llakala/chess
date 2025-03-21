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

const num_rows = 8

const num_cols = 8

// TODO: consider making opaque and using a get_data function
pub type Board {
  Board(data: Array(Piece))
}

/// Create empty board
pub fn empty() -> Board {
  let none = piece.None
  iv.initialise(num_rows * num_cols, fn(_) { none }) |> Board
}

/// Create a board with some initial data
/// Returns an error if the data was of an invalid length
pub fn create(data: Array(Piece)) -> Result(Board, String) {
  let size = num_rows * num_cols
  let length = data |> iv.length
  use <- bool.guard(
    length == size,
    Error(
      "Boards can only be created with data of length `"
      <> size |> int.to_string
      <> "`, but data was of length `"
      <> length |> int.to_string
      <> "`!",
    ),
  )
  data |> Board |> Ok
}

/// Returns the piece at the given coordinate
/// Will return an error if the position is invalid
pub fn get_pos(board: Board, pos: Position) -> Result(Piece, String) {
  // Error out early if the position is invalid
  let pos_valid = pos_is_valid(pos)
  use _ <- result.try(pos_valid)

  // 0 corresponds to top left of board
  let index: Int = position.get_index(pos)
  let length = board.data |> iv.length
  use <- bool.guard(
    index >= length,
    Error(
      "Attempted to set a value of the board at index `"
      <> index |> int.to_string
      <> "`, but the board is only of length `"
      <> length |> int.to_string
      <> "` !",
    ),
  )

  use piece: Piece <- result.try(
    board.data |> iv.get(index) |> result.replace_error("Index invalid!"),
  )

  piece |> Ok
}

/// Set the board's position at the given coordinate
/// Return the new board or an error
pub fn set_pos(
  board: Board,
  pos: Position,
  piece: Piece,
) -> Result(Board, String) {
  // Error out early if the position is invalid
  use _ <- result.try(pos_is_valid(pos))

  // 0 corresponds to top left of board
  let index: Int = position.get_index(pos)
  let length = board.data |> iv.length

  use <- bool.guard(
    index >= length,
    Error(
      "Attempted to set a value of the board at index `"
      <> index |> int.to_string
      <> "`, but the board is only of length `"
      <> length |> int.to_string
      <> "` !",
    ),
  )

  use data <- result.try(
    board.data
    |> iv.set(index, piece)
    |> result.replace_error(
      "Failed to set value at index `" <> index |> int.to_string <> "`!",
    ),
  )
  data |> Board |> Ok
}

pub fn to_string(board: Board) -> Result(String, String) {
  board.data
  |> iv.map(piece.to_string)
  |> format_list(num_rows)
}

fn pos_is_valid(pos: Position) -> Result(Nil, String) {
  let row = pos.row
  let col = pos.col
  use <- bool.guard(
    row >= num_rows,
    Error(
      "Tried to access row index `"
      <> row |> int.to_string
      <> "`, but the board only had `"
      <> num_rows |> int.to_string
      <> "` rows!",
    ),
  )
  use <- bool.guard(
    col >= num_cols,
    Error(
      "Tried to access column index `"
      <> col |> int.to_string
      <> "`, but the board only had `"
      <> num_cols |> int.to_string
      <> "` columns!",
    ),
  )
  Ok(Nil)
}

/// Doesn't take the *entire* fen string: just the first part encoding the board
pub fn from_fen(fen: String) -> Result(Board, String) {
  use pieces <- result.try(
    fen |> string.split("") |> list.try_map(piece.from_fen),
  )

  pieces |> iv.from_list |> Board(8, 8, _) |> Ok
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
