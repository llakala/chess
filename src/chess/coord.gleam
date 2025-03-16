import gleam/bool
import gleam/int
import gleam/result
import gleam/string

// We asume we'll be playing chess, so rows/cols should be 8 long
const row_len: Int = 8

const col_len: Int = 8

// TODO: Consider making opaque to prevent direct access
/// This shouldn't be accessed directly. Rather, the `coord.from_pair` or `coord.from_index` functinos should be used, as they check for invalid inputs.
pub type Coord {
  Coord(col: Int, row: Int, index: Int)
}

/// Generate a new coord based on a row and column.
/// Assumes a row length of 8, and errors if it received value outside of that
pub fn from_pair(col col: Int, row row: Int) -> Result(Coord, String) {
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

  let index: Int = row * row_len + col

  Coord(col, row, index) |> Ok
}

// TODO: Catch error if `index > 63`
/// Generate a new coord based on an index
/// Assumes a row length of 8
pub fn from_index(index: Int) -> Coord {
  let col: Int = index % row_len
  let row: Int = index / row_len

  Coord(col, row, index)
}

/// Returns a string representation of the coordinate, showing its row and column
///
/// ## Examples
///
/// ```gleam
/// coord.new(2,3) |> to_string
/// // -> "(2,3)"
/// ```
pub fn to_string(pos: Coord) -> String {
  "(" <> int.to_string(pos.col) <> ", " <> int.to_string(pos.row) <> ")"
}

pub fn to_algebraic(coord: Coord) -> String {
  let row = coord.row
  let col = coord.col

  // We don't check errors, under the expectation that `coord.from_pair`
  // is how a coordinate is initialized, and errors are checked there.
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
    _ -> "ERROR"
    // Just in case
  }
  file <> rank
}

/// Get a Coord from algebraic notation. Returns an error if the algebraic notation was invalid
pub fn from_algebraic(str: String) -> Result(Coord, String) {
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

  // No need to pipe into Ok, from_pair returns a result already. `result.try` means if we got an error earlier, it's already been handled
  from_pair(col, row)
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
    1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 -> rank_value - 1 |> Ok
    _ ->
      Error(
        "Invalid algebraic notation rank `"
        <> rank
        <> "` passed! Ranks are only expected to be 1-8.",
      )
  }
}
