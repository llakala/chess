import chess/constants.{num_rows}
import gleam/int
import gleam/result

pub fn parse(row_str: String) -> Result(Int, String) {
  use row <- result.try(
    int.parse(row_str)
    |> result.replace_error(
      "Expected numeric input, but got " <> row_str <> ".",
    ),
  )
  case row >= 1 && row <= num_rows {
    // We store it as 0-based internally
    True -> row - 1 |> Ok

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

/// Given some index, either return it unchanged, or error.
pub fn validate(index: Int) -> Result(Int, String) {
  case index >= 0 && index < num_rows {
    // No need to subtract, it's 0-based inside
    True -> index |> Ok

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
pub fn to_string(value: Int) -> String {
  value + 1 |> int.to_string
}
