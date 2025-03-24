import chess/constants.{col_len, row_len}
import chess/file.{type File}
import chess/rank.{type Rank}

import gleam/bool
import gleam/result
import gleam/string

pub type Position {
  Position(file: File, rank: Rank)
}

/// Generate a new position based on a 0-based column and row index.
/// Errors if it receives value outside of the row/col length
pub fn new(col col: Int, row row: Int) -> Result(Position, String) {
  use rank <- result.try(row |> rank.from_index)
  use file <- result.try(col |> file.from_index)

  Position(rank:, file:) |> Ok
}

/// Get the index of a position, oriented so it's intuitive as white.
/// This does NOT give you the index of a position in the data. instead, (0, 0) corresponds to the bottom left of the data here. This is for easy
/// conversions between Position and index
pub fn to_player_index(pos: Position) {
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

/// Get a position from algebraic notation. Returns an error if the algebraic notation was invalid
pub fn from_algebraic(str: String) -> Result(Position, String) {
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
