import gleam/bool
import gleam/int
import gleam/result
import gleam/string

// We asume we'll be playing chess, so rows should be 8 long
const row_len: Int = 8

pub type Coord {
  Coord(col: Int, row: Int, index: Int)
}

/// Generate a new coord based on a row and column.
/// Assumes a row length of 8
pub fn from_pair(col col: Int, row row: Int) -> Coord {
  let index: Int = row * row_len + col

  Coord(col, row, index)
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
/// coord.new(2,3) |> to_string
/// // -> "(2,3)"
pub fn to_string(pos: Coord) -> String {
  "(" <> int.to_string(pos.col) <> ", " <> int.to_string(pos.row) <> ")"
}

/// TODO
pub fn from_algebraic(str) {
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
  from_pair(col, row)
  |> Ok
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
