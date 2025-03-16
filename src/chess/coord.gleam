import gleam/int

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
