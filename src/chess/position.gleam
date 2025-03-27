import chess/constants.{col_len, num_cols, num_rows, row_len}
import chess/direction.{
  type Direction, Down, DownLeft, DownRight, Left, Right, Up, UpLeft, UpRight,
}
import chess/file.{type File}
import chess/rank.{type Rank}
import gleam/int

import gleam/bool
import gleam/result
import gleam/string

pub type Position {
  Position(file: File, rank: Rank)
}

/// Get a position from algebraic notation. Returns an error if the algebraic notation was invalid
pub fn new(str: String) -> Result(Position, String) {
  use <- bool.guard(string.length(str) != 2, Error("Invalid string!"))

  use #(file_str, rank_str) <- result.try(
    str
    |> string.pop_grapheme
    |> result.replace_error(
      "Somehow, the popped grapheme wasn't of length 2, even though I already checked for that!",
    ),
  )

  use file <- result.try(file_str |> file.new)
  use rank <- result.try(rank_str |> rank.new)

  // The `new()` function expects integers, not Ranks and Files. We use
  // the record directly, since we're within the type, and we already
  // validated both the rank and file. A function to handle this kind of
  // thing in the future might be nice, but it's fine for now.
  Position(rank:, file:) |> Ok
}

/// Generate a new position based on a 0-based column and row index.
/// Errors if it receives value outside of the row/col length
pub fn from_index(col col: Int, row row: Int) -> Result(Position, String) {
  use rank <- result.try(row |> rank.from_index)
  use file <- result.try(col |> file.from_index)

  Position(rank:, file:) |> Ok
}

/// Takes a classical index (0 being the top left) and turn it into a
/// Position.
pub fn from_data_index(index: Int) -> Result(Position, String) {
  let row = index / row_len
  let col = index % row_len

  from_index(row:, col:)
}

/// Get the index of a position, oriented so it's intuitive as white.
/// This does NOT give you the index of a position in the data. instead, (0, 0) corresponds to the bottom left of the data here. This is for easy
/// conversions between Position and index
pub fn to_index(pos: Position) {
  let row = pos.rank |> rank.to_index
  let col = pos.file |> file.to_index

  let bottom_left = { row_len * col_len } - { 1 * row_len }

  bottom_left - { row * row_len } + col
}

pub fn to_algebraic(pos: Position) -> String {
  let file = pos.file |> file.to_string
  let rank = pos.rank |> rank.to_string

  file <> rank
}

/// Return the distance to the edge of the board when moving in a given
/// direction.
pub fn distance_to_edge(pos: Position, dir: Direction) {
  let down_dist = pos.rank |> rank.to_index
  let up_dist = num_rows - 1 - down_dist

  let left_dist = pos.file |> file.to_index
  let right_dist = num_cols - 1 - left_dist

  case dir {
    Up -> up_dist
    Down -> down_dist
    Left -> left_dist
    Right -> right_dist

    UpLeft -> int.min(up_dist, left_dist)
    UpRight -> int.min(up_dist, right_dist)
    DownLeft -> int.min(down_dist, left_dist)
    DownRight -> int.min(down_dist, right_dist)
  }
}
