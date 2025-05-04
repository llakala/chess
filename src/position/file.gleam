import chess/constants
import gleam/bool
import gleam/int
import gleam/string

/// Given some string, parse it into the correct integer value. "a" -> 0,
/// "b" -> 1, etc.
pub fn parse(file: String) -> Result(Int, String) {
  use <- bool.guard(
    string.contains(does: "abcdefgh", contain: file) == False,
    Error(
      "Invalid algebraic notation file `"
      <> file
      <> "` passed! Files are only expected to be a-h.",
    ),
  )

  case file {
    "a" -> 0
    "b" -> 1
    "c" -> 2
    "d" -> 3
    "e" -> 4
    "f" -> 5
    "g" -> 6
    "h" -> 7

    // This is impossible since the type is opaque, and we handled this
    // error above. Gotta handle it anyways. I wish the compiler understood bool.guard!
    _ -> -1
  }
  |> Ok
}

/// Given some index, either return it unchanged, or error out if it's invalid
pub fn validate(index: Int) -> Result(Int, String) {
  use <- bool.guard(
    index < 0 || index >= constants.num_cols,
    Error(
      "Invalid index `"
      <> index |> int.to_string
      <> "` passed! File indices are only expected to be 0-7.",
    ),
  )

  index |> Ok
}

pub fn to_string(value: Int) -> String {
  case value {
    0 -> "a"
    1 -> "b"
    2 -> "c"
    3 -> "d"
    4 -> "e"
    5 -> "f"
    6 -> "g"
    7 -> "h"

    // Impossible, since files are opaque and guarded against being anything
    // except 0.7. Handle it anyways.
    _ -> "z"
  }
}
