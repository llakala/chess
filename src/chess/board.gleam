// My own helper functions for working with `iv` arrays
import gleam/bool
import gleam/int
import gleam/result
import gleam/string

import chess/color.{type Color}
import chess/constants.{col_len, num_cols, num_rows, row_len}
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
  let assert Ok(board) = from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")

  board
}

/// Create a board with some initial data
/// Returns an error if the data was of an invalid length
pub fn create(data: Array(Square)) -> Result(Board, String) {
  let size = row_len * col_len
  let length = data |> iv.length

  use <- bool.guard(
    length != size,
    Error(
      "Boards can only be created with data of length `"
      <> size |> int.to_string
      <> "`, but data was of length `"
      <> length |> int.to_string
      <> "`!",
    ),
  )

  Board(data) |> Ok
}

/// Getter function to access the board's data, since boards are
/// opaque
pub fn get_data(board: Board) -> Array(Square) {
  board.data
}

/// Returns the piece at the given coordinate
/// We don't have to validate that the position is valid, since pos
/// is an opaque type and checked on creation
pub fn get_pos(board: Board, pos: Position) -> Square {
  // 0 corresponds to bottom left of board, in relation to the player
  let index: Int = position.to_index(pos)

  // If this ever fails, it's a logic error, not a user issue
  let assert Ok(square) = board.data |> iv.get(index)

  square
}

/// Set the board's position at the given coordinate
/// Doesn't need to return an error - all of these are
/// opaque types that must be valid to be created
pub fn set_pos(board: Board, pos: Position, square: Square) -> Board {
  // 0 corresponds to bottom left of the board, in relation to the player
  let index: Int = position.to_index(pos)

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
/// a piece in a direction.
pub fn obstructed_distance(
  board board: Board,
  position position: Position,
  direction direction: Direction,
  color color: Color,
) -> Int {
  obstructed_distance_loop(board, position, direction, color, 0)
}

fn obstructed_distance_loop(
  board: Board,
  pos: Position,
  dir: Direction,
  color: Color,
  accumulated_distance: Int,
) -> Int {
  // Get the next posiiton in the direction. If from_offset returns an error, we've
  // gone too far and gone off the board edge -- return the accumulated distance
  // immediately
  use new_pos <- choose.cases(
    position.from_offset(pos, 1, dir),
    on_error: fn(_) { accumulated_distance },
  )

  // The error should've already been checked above.
  let square = get_pos(board, new_pos)

  // Convert the square into a piece. If `to_piece` returns an error, the square
  // must've been empty. In that case, simply keep the loop going, adding 1 to the
  // accumulated distance
  use piece <- choose.cases(square |> square.to_piece, on_error: fn(_) {
    obstructed_distance_loop(
      board,
      new_pos,
      dir,
      color,
      accumulated_distance + 1,
    )
  })

  // Whether the color of the piece we started at equals the color of the
  // current piece we found when traveling that direction
  case color == piece.color {
    // Other piece is an enemy and can be captured - so it's the max distance
    // we can go in that direction.
    False -> accumulated_distance + 1

    // Other piece is a friend - can't capture it. Stop at our current distance.
    True -> accumulated_distance
  }
}

/// Doesn't take the *entire* fen string: just the first part encoding the board
pub fn from_fen(fen: String) -> Result(Board, String) {
  // Important to start with an empty board, so if we skip some indices,
  // they'll just be filed with null
  let initial = empty().data |> Board

  from_fen_loop(fen, initial, 0, 0)
}

fn from_fen_loop(
  fen: String,
  board: Board,
  col: Int,
  row: Int,
) -> Result(Board, String) {
  let res = string.pop_grapheme(fen)

  // If we pop the grapheme and get an error, the fen string is over
  // Reverse the array, since popping the grapheme puts things in the
  // reverse order
  use <- bool.guard(
    res |> result.is_error,
    board.data |> iv.reverse |> Board |> Ok,
  )

  // Don't know of a better way to do this. We've already mapped the error away, so now what?
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
    from_fen_loop(rest, board, col + skippable, row)
  })

  let is_slash = cur == "/"
  let row_done = col > 7

  case is_slash, row_done {
    False, True -> Error("Fen forgot to end the row!")

    True, False -> Error("Fen ended too early at col " <> col |> int.to_string)

    // We reached the row separator - move onto the next
    // row, just without the `/`
    True, True -> from_fen_loop(rest, board, 0, row + 1)

    // We can keep going in our current row
    False, False -> {
      use piece <- result.try(piece.from_fen(cur))
      let square = square.Some(piece)

      use pos <- result.try(position.from_index(col, row))
      let new_board = set_pos(board, pos, square)

      from_fen_loop(rest, new_board, col + 1, row)
    }
  }
}
