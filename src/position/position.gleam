import chess/constants.{col_len, row_len}
import chess/sliding.{type Direction}
import gleam/order.{type Order}
import position/file.{type File}
import position/offset.{type Offset}
import position/rank.{type Rank}

import gleam/bool
import gleam/result
import gleam/string

pub opaque type Position {
  Position(file: File, rank: Rank)
}

/// Get a position from algebraic notation. Returns an error if the algebraic notation was invalid
pub fn new(fen fen: String) -> Result(Position, String) {
  use <- bool.guard(string.length(fen) != 2, Error("Invalid string!"))

  use #(file_str, rank_str) <- result.try(
    fen
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
pub fn from_indices(col col: Int, row row: Int) -> Result(Position, String) {
  use rank <- result.try(row |> rank.from_index)
  use file <- result.try(col |> file.from_index)

  Position(rank:, file:) |> Ok
}

pub fn apply_offset(pos: Position, offset: Offset) -> Result(Position, String) {
  let file = pos.file |> file.to_index
  // Changing the file moves you horizontally
  let file_change = offset.horizontal

  let rank = pos.rank |> rank.to_index
  // Changing the rank moves you vertically
  let rank_change = offset.vertical

  from_indices(file + file_change, rank + rank_change)
}

/// Take an existing position, a distance, and a direction, and return a new
/// position. This will fail if the direction went off the board - it's recommended
/// to use `move.obstructed_distance` to find the maximum distance for a given
/// direction that one can go in an actual game (inclusive of captures)
pub fn in_direction(
  position pos: Position,
  distance dist: Int,
  direction dir: Direction,
) -> Result(Position, String) {
  let offset = offset.in_direction(dir, dist)
  apply_offset(pos, offset)
}

/// Takes a classical index (0 being the top left) and turn it into a
/// Position.
pub fn from_data_index(index index: Int) -> Result(Position, String) {
  // Flip the row around, so it correctly starts in the bottom left
  let row = constants.num_rows - 1 - { index / row_len }
  let col = index % row_len

  from_indices(row:, col:)
}

/// Get the index of a position, oriented so it's intuitive as white. This does
/// NOT give you the index of a position in the data. instead, (0, 0) corresponds
/// to the bottom left of the data here. This is for easy conversions between
/// Position and index
pub fn to_player_index(position pos: Position) -> Int {
  let row = pos.rank |> rank.to_index
  let col = pos.file |> file.to_index

  let bottom_left = { row_len * col_len } - { 1 * row_len }

  bottom_left - { row * row_len } + col
}

pub fn to_data_index(position pos: Position) -> Int {
  let row = constants.num_rows - 1 - { pos.rank |> rank.to_index }
  let col = pos.file |> file.to_index

  row * row_len + col
}

pub fn to_string(position pos: Position) -> String {
  let file = pos.file |> file.to_string
  let rank = pos.rank |> rank.to_string

  file <> rank
}

/// Returns a pair that looks like #(rank, file). 0-based indexing.
pub fn to_indices(position pos: Position) -> #(Int, Int) {
  let rank_index = pos.rank |> rank.to_index
  let file_index = pos.file |> file.to_index

  #(rank_index, file_index)
}

/// Get the rank of the position, typically thought of as the row
pub fn get_rank(position pos: Position) -> Rank {
  pos.rank
}

/// Get the file of the position, typically thought of as the column
pub fn get_file(position pos: Position) -> File {
  pos.file
}

/// Sorts two positions first based on the ranks, then falling back to the file.
///
/// (a1, b1) -> Lt
/// (a1, a2) -> Lt
/// (a8, b1) -> Lt
pub fn compare(pos1 pos1: Position, pos2 pos2: Position) -> Order {
  let assert [rank1, file1] = pos1 |> to_string |> string.split("")
  let assert [rank2, file2] = pos2 |> to_string |> string.split("")

  // Only keep going if rank1 and rank2 are the same.
  use <- order.lazy_break_tie(string.compare(rank1, rank2))
  string.compare(file1, file2)
}
