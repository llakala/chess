import gleam/bool
import gleam/int
import gleam/result
import gleam/string

// TODO: Consider making opaque to prevent direct access
/// This shouldn't be accessed directly. Rather, the `position.new` function should be used, as it checks for invalid inputs.
pub type Position {
  Position(col: Int, row: Int)
}

// We asume we'll be playing chess, so rows/cols should be 8 long
const row_len: Int = 8

const col_len: Int = 8

/// Generate a new position based on a 0-based column and row index.
/// Assumes a row/col length of 8, and errors if it received value outside of that
pub fn new(col col: Int, row row: Int) -> Result(Position, String) {
  use <- bool.guard(
    col > col_len - 1 && col > 0,
    Error(
      "Invalid column `"
      <> col |> int.to_string
      <> "` passed. Columns can only have value 0-7",
    ),
  )

  use <- bool.guard(
    row > row_len - 1 && row > 0,
    Error(
      "Invalid row `"
      <> row |> int.to_string
      <> "` passed. Rows can only have value 0-7",
    ),
  )

  Position(col, row) |> Ok
}

/// Get the index of a position. 0 corresponds to the top left corner, and 63 corresponds to the bottom right.
pub fn get_index(pos: Position) {
  pos.row * row_len + pos.col
}

pub fn to_algebraic(pos: Position) -> String {
  let row = pos.row
  let col = pos.col

  // We don't check errors, under the expectation that `new()`
  // is how a position is initialized, and errors are checked there.
  // Not using the function and calling the record itself is a skill issue,
  // and I'm not going to make the type opaque just to protect you from yourself
  let rank = row |> int.add(1) |> int.to_string
  let file = case col {
    0 -> "a"
    1 -> "b"
    2 -> "c"
    3 -> "d"
    4 -> "e"
    5 -> "f"
    6 -> "g"
    7 -> "h"

    // Just in case
    _ -> "ERROR"
  }

  file <> rank
}

/// Get a position from algebraic notation. Returns an error if the algebraic notation was invalid
pub fn from_algebraic(str: String) -> Result(Position, String) {
  use <- bool.guard(string.length(str) != 2, Error("Invalid string!"))

  use #(file, rank) <- result.try(
    str
    |> string.pop_grapheme
    |> result.replace_error(
      "Somehow, the popped grapheme wasn't of length 2, even though I already checked for that!",
    ),
  )

  use col <- result.try(file |> parse_file)
  use row <- result.try(rank |> parse_rank)

  // No need to pipe into Ok, `new()` returns a result already. `result.try` means if we got an error earlier, it's already been handled
  new(col, row)
}

/// Parse the algebraic notation for a file, and return an error if it's invalid
fn parse_file(file: String) -> Result(Int, String) {
  case file {
    "a" -> 0 |> Ok
    "b" -> 1 |> Ok
    "c" -> 2 |> Ok
    "d" -> 3 |> Ok
    "e" -> 4 |> Ok
    "f" -> 5 |> Ok
    "g" -> 6 |> Ok
    "h" -> 7 |> Ok

    _ ->
      Error(
        "Invalid algebraic notation file `"
        <> file
        <> "` passed! Files are only expected to be a-h.",
      )
  }
}

/// Parse the algebraic notation for a rank, and return an error if it's invalid
fn parse_rank(rank: String) -> Result(Int, String) {
  use rank_value <- result.try(
    rank
    |> int.parse
    |> result.replace_error("Encountered non-numeric row `" <> rank <> "`!"),
  )

  case rank_value {
    // Subtract one to make the index 0-based
    1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 -> Ok(rank_value - 1)

    _ ->
      Error(
        "Invalid algebraic notation rank `"
        <> rank
        <> "` passed! Ranks are only expected to be 1-8.",
      )
  }
}
