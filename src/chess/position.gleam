import chess/constants.{col_len, row_len}
import chess/file.{type File}
import chess/rank.{type Rank}
import chess/sliding.{
  type Direction, Down, DownLeft, DownRight, Left, Right, Up, UpLeft, UpRight,
}

import gleam/bool
import gleam/result
import gleam/string

pub opaque type Position {
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

/// Take an existing position, a distance, and a direction, and return a new
/// position. This will fail if the direction went off the board - it's recommended
/// to use `move.obstructed_distance` to find the maximum distance for a given
/// direction that one can go in an actual game (inclusive of captures)
pub fn from_offset(
  pos: Position,
  distance: Int,
  direction: Direction,
) -> Result(Position, String) {
  let row = pos.rank |> rank.to_index
  let col = pos.file |> file.to_index
  case direction {
    Up -> from_index(row: row + distance, col: col)
    Down -> from_index(row: row - distance, col: col)
    Right -> from_index(row:, col: col + distance)
    Left -> from_index(row:, col: col - distance)
    UpRight -> from_index(row: row + distance, col: col + distance)
    UpLeft -> from_index(row: row + distance, col: col - distance)
    DownRight -> from_index(row: row - distance, col: col + distance)
    DownLeft -> from_index(row: row - distance, col: col - distance)
  }
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
