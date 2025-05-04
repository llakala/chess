import chess/constants
import gleam/int
import gleam/result

pub fn parse(row_str: String) -> Result(Int, String) {
  use row <- result.try(
    int.parse(row_str)
    |> result.replace_error(
      "Expected numeric input, but got " <> row_str <> ".",
    ),
  )
  case row >= 1 && row <= constants.num_rows {
    // We store it internally with 0 corresponding to the top left, not the
    // bottom left.
    True -> Ok(constants.num_rows - row)

    False ->
      Error(
        "Invalid algebraic notation rank `"
        <> row |> int.to_string
        <> "` passed! Ranks are only expected to be 1-"
        <> constants.num_rows |> int.to_string
        <> ".",
      )
  }
}

/// Given some index, either return it unchanged, or error.
pub fn validate(index: Int) -> Result(Int, String) {
  case index >= 0 && index < constants.num_rows {
    // No need to subtract, it's 0-based inside
    True -> index |> Ok

    _ ->
      Error(
        "Invalid index `"
        <> index |> int.to_string
        <> "` passed! Rank indices are only expected to be 0-"
        <> constants.num_rows - 1 |> int.to_string
        <> ".",
      )
  }
}

/// Returns a 1-based index representing the row, for working with algebraic notation
pub fn to_string(value: Int) -> String {
  int.to_string(constants.num_ranks - value)
}
