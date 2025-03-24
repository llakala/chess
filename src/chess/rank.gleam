import chess/constants.{num_rows}
import gleam/int
import gleam/result

/// Typically thought of as a row
pub opaque type Rank {
  Rank(value: Int)
}

/// Create a rank from a numeric string. Errors if the index was outside
/// of the bounds, or the string wasn't numeric! Use this if you're
/// working with input from algebraic notation that'll be 1-based.
pub fn new(row_str: String) -> Result(Rank, String) {
  use row <- result.try(
    int.parse(row_str)
    |> result.replace_error(
      "Expected numeric input, but got " <> row_str <> ".",
    ),
  )
  case row >= 1 && row <= num_rows {
    // We store it as 0-based internally
    True -> row - 1 |> Rank |> Ok

    False ->
      Error(
        "Invalid algebraic notation rank `"
        <> row |> int.to_string
        <> "` passed! Ranks are only expected to be 1-"
        <> num_rows |> int.to_string
        <> ".",
      )
  }
}

pub fn from_index(index: Int) -> Result(Rank, String) {
  case index >= 0 && index < num_rows {
    // No need to subtract, it's 0-based inside
    True -> index |> Rank |> Ok

    _ ->
      Error(
        "Invalid index `"
        <> index |> int.to_string
        <> "` passed! Rank indices are only expected to be 0-"
        <> num_rows - 1 |> int.to_string
        <> ".",
      )
  }
}

/// Returns a 1-based index representing the row, for working with algebraic notation
pub fn to_string(rank: Rank) -> String {
  rank.value + 1 |> int.to_string
}

/// Returns a 0-based index for working with board data
pub fn to_index(rank: Rank) -> Int {
  rank.value
}
