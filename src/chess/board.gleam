// My own helper functions for working with `iv` arrays
import gleam/bool
import gleam/int
import gleam/result
import gleam/string

import chess/color.{type Color}
import chess/constants.{num_cols, num_rows}
import chess/piece
import chess/position.{type Position}
import chess/sliding.{type Direction}
import chess/square.{type Square}

import utils/array
import utils/choose

import iv.{type Array}

pub opaque type Board {
  Board(data: Array(Square))
}

pub type Distance {
  NonCapture(distance: Int)
  Capture(distance: Int)
}

/// Create empty board
pub fn empty() -> Board {
  let none = square.None
  let data = iv.initialise(num_rows * num_cols, fn(_) { none })

  Board(data)
}

/// Create the beginning chess arrangement, with all the pieces on their
/// starting squares
pub fn initial() -> Board {
  // This is hardcoded, so it should always work
  // If it fails, from_fen should have failed too
  // Maybe I should be handling the error better, but truthfully
  // I just want a full board
  let assert Ok(board) = new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")

  board
}

/// Doesn't take the *entire* fen string: just the first part encoding the board
pub fn new(fen: String) -> Result(Board, String) {
  // Important to start with an empty board, so if we skip some indices,
  // they'll just be filed with null
  let initial = empty().data

  from_fen_loop(fen, initial, 0, 0)
}

/// Returns the piece at the given coordinate
/// We don't have to validate that the position is valid, since pos
/// is an opaque type and checked on creation
pub fn get_pos(board: Board, pos: Position) -> Square {
  // 0 corresponds to bottom left of board, in relation to the player
  let index: Int = position.to_player_index(pos)

  // If this ever fails, it's a logic error, not a user issue
  let assert Ok(square) = board.data |> iv.get(index)

  square
}

/// Set the board's position at the given coordinate
/// Doesn't need to return an error - all of these are
/// opaque types that must be valid to be created
pub fn set_pos(board: Board, pos: Position, square: Square) -> Board {
  // 0 corresponds to bottom left of the board, in relation to the player
  let index: Int = position.to_player_index(pos)

  let assert Ok(new_data) =
    board.data
    |> iv.set(index, square)

  Board(new_data)
}

pub fn to_string(board: Board) -> String {
  // data represented as a list of rows, each of which is a list of single characters
  // We use `let assert` since board is opaque, so the data should always be of
  // length 64
  let assert Ok(data_rows) =
    array.sized_chunk(board.data |> iv.map(square.to_string), num_rows)

  data_rows
  // Join each element in a row together with comma separators
  |> iv.map(array.join(_, ", "))
  // Join each row together with newlines
  |> array.join("\n")
}

/// Return the number of squares in a direction until you either bump into a wall
/// or hit another piece. Useful for determining the number of valid moves for
/// a piece in a direction. Returns a custom type Distance, so you can tell if there
/// was a capture in that direction
pub fn obstructed_distance(
  board board: Board,
  position position: Position,
  direction direction: Direction,
  color color: Color,
) -> Distance {
  obstructed_distance_loop(board, position, direction, color, 0)
}

fn obstructed_distance_loop(
  board: Board,
  pos: Position,
  dir: Direction,
  color: Color,
  distance: Int,
) -> Distance {
  // Distance next position in the direction. If from_offset returns an error, we've
  // gone too far and gone off the board edge -- return the accumulated distance
  // immediately, without a capture since we never hit one
  use new_pos <- choose.cases(
    position.in_direction(pos, 1, dir),
    on_error: fn(_) { NonCapture(distance) },
  )

  let square = get_pos(board, new_pos)

  // Convert the square into a piece. If `to_piece` returns an error, the square
  // must've been empty. In that case, simply keep the loop going, adding 1 to the
  // accumulated distance
  use piece <- choose.cases(square |> square.to_piece, on_error: fn(_) {
    obstructed_distance_loop(board, new_pos, dir, color, distance + 1)
  })

  // Whether the color of the piece we started at equals the color of the current
  // piece we found when traveling that direction
  case color == piece.color {
    // Other piece is an enemy and can be captured - return a Capture, which
    // indicates that this is the distance to a capture, and you can subtract
    // one to get the distance to a non-capture.
    False -> Capture(distance + 1)

    // Other piece is a friend - can't capture it. Note that `distance` never gets
    // modified, so this will be the distance to the current square, not the new
    // square.
    True -> NonCapture(distance)
  }
}

fn from_fen_loop(
  fen: String,
  data: Array(Square),
  col: Int,
  row: Int,
) -> Result(Board, String) {
  let res = string.pop_grapheme(fen)

  // If we pop the grapheme and get an error, the fen string is over
  use <- bool.guard(res |> result.is_error, data |> Board |> Ok)

  // Yeah yeah, I know `bool.guard` followed by an assertion means you should be
  // using `case`, but let me have my antipatterns, okay?
  let assert Ok(#(cur, rest)) = res

  // If we parse num and get Ok, it means we have to skip some of the rest
  // of the row. Handle it early and leave the rest of the code to handle
  // the Error case - where we actually have a piece to handle
  use _ <- choose.cases(int.parse(cur), on_ok: fn(skippable) {
    use <- bool.guard(
      col + skippable > 8,
      Error("Was told to skip more squares than there were left in the row!"),
    )

    // It's fine to just skip the empty spaces, since we initialize the
    // list to be full of None
    from_fen_loop(rest, data, col + skippable, row)
  })

  let is_slash = cur == "/"
  let row_done = col > 7

  case is_slash, row_done {
    False, True -> Error("Fen forgot to end the row!")

    True, False -> Error("Fen ended too early at col " <> col |> int.to_string)

    // We reached the row separator - move onto the next
    // row, just without the `/`
    True, True -> from_fen_loop(rest, data, 0, row + 1)

    // We can keep going in our current row
    False, False -> {
      use piece <- result.try(piece.from_fen(cur))
      let square = square.Some(piece)

      let index = row * constants.row_len + col

      let assert Ok(data) = data |> iv.set(index, square)

      from_fen_loop(rest, data, col + 1, row)
    }
  }
}
