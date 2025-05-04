import chess/constants
import gleam/order.{type Order}
import position/direction.{type Direction}
import position/file
import position/offset.{type Offset}
import position/rank

import gleam/bool
import gleam/result
import gleam/string

// Internal index is stored with 0 corresponding to the top left.
pub opaque type Position {
  Position(index: Int)
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

  use file <- result.try(file_str |> file.parse)
  use rank <- result.try(rank_str |> rank.parse)

  from_indices(file:, rank:)
}

/// Generate a new position based on a 0-based file and rank index. Bottom-left
/// oriented. Errors if it receives value outside of the row/col length
pub fn from_indices(file file: Int, rank rank: Int) -> Result(Position, String) {
  use rank <- result.try(rank |> rank.validate)
  use file <- result.try(file |> file.validate)

  Position(rank * constants.rank_len + file) |> Ok
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

pub fn apply_offset(pos: Position, offset: Offset) -> Result(Position, String) {
  let file = file_index(pos)
  // Changing the file moves you horizontally
  let file_change = offset.horizontal

  let rank = rank_index(pos)
  // Changing the rank moves you vertically
  let rank_change = offset.vertical

  // Subtract for the rank change, because our internal index is based with 0 at
  // the top
  from_indices(file + file_change, rank - rank_change)
}

/// Takes a classical index (0 being the top left) and turn it into a
/// Position.
pub fn from_index(index index: Int) -> Result(Position, String) {
  // TODO OPTIMIZE
  let rank = index / constants.num_ranks
  let file = index % constants.num_files

  from_indices(file:, rank:)
}

/// Get a data-oriented index, with 0 corresponding to the top left.
pub fn to_index(position pos: Position) -> Int {
  let rank = rank_index(pos)
  let file = file_index(pos)

  rank * constants.rank_len + file
}

pub fn to_string(position pos: Position) -> String {
  let file_str = file_index(pos) |> file.to_string
  let rank_str = rank_index(pos) |> rank.to_string

  file_str <> rank_str
}

/// Returns a pair that looks like #(rank, file). 0-based indexing, oriented
/// with 0 corresponding to the bottom row.
pub fn to_indices(position pos: Position) -> #(Int, Int) {
  #(rank_index(pos), file_index(pos))
}

/// Returns the rank oriented for the data - with 0 corresponding to the top
pub fn rank_index(position pos: Position) -> Int {
  pos.index / constants.num_ranks
}

pub fn friendly_rank_index(position pos: Position) -> Int {
  constants.num_ranks - 1 - { pos.index / constants.num_ranks }
}

pub fn file_index(position pos: Position) -> Int {
  pos.index % constants.num_files
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
