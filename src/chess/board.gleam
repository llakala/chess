// My own helper functions for working with `iv` arrays
import chess/array
import chess/choose
import gleam/int

import chess/piece.{type Piece}
import chess/position.{type Position}

import gleam/bool
import gleam/result
import gleam/string

import iv.{type Array}

const num_rows = 8

const num_cols = 8

// TODO: consider making opaque and using a get_data function
pub type Board {
  Board(data: Array(Piece))
}

/// Create empty board
pub fn empty() -> Board {
  let none = piece.None
  iv.initialise(num_rows * num_cols, fn(_) { none }) |> Board
}

/// Create a board with some initial data
/// Returns an error if the data was of an invalid length
pub fn create(data: Array(Piece)) -> Result(Board, String) {
  let size = num_rows * num_cols
  let length = data |> iv.length
  use <- bool.guard(
    length == size,
    Error(
      "Boards can only be created with data of length `"
      <> size |> int.to_string
      <> "`, but data was of length `"
      <> length |> int.to_string
      <> "`!",
    ),
  )

  data |> Board |> Ok
}

/// Returns the piece at the given coordinate
/// Will return an error if the position is invalid
pub fn get_pos(board: Board, pos: Position) -> Result(Piece, String) {
  // Error out early if the position is invalid
  let pos_valid = pos_is_valid(pos)
  use _ <- result.try(pos_valid)

  // 0 corresponds to top left of board
  let index: Int = position.get_index(pos)
  let length = board.data |> iv.length

  use <- bool.guard(
    index >= length,
    Error(
      "Attempted to set a value of the board at index `"
      <> index |> int.to_string
      <> "`, but the board is only of length `"
      <> length |> int.to_string
      <> "` !",
    ),
  )

  use piece: Piece <- result.try(
    board.data |> iv.get(index) |> result.replace_error("Index invalid!"),
  )

  piece |> Ok
}

/// Set the board's position at the given coordinate
/// Return the new board or an error
pub fn set_pos(
  board: Board,
  pos: Position,
  piece: Piece,
) -> Result(Board, String) {
  // Error out early if the position is invalid
  use _ <- result.try(pos_is_valid(pos))

  // 0 corresponds to top left of board
  let index: Int = position.get_index(pos)
  let length = board.data |> iv.length

  use <- bool.guard(
    index >= length,
    Error(
      "Attempted to set a value of the board at index `"
      <> index |> int.to_string
      <> "`, but the board is only of length `"
      <> length |> int.to_string
      <> "` !",
    ),
  )

  use data <- result.try(
    board.data
    |> iv.set(index, piece)
    |> result.replace_error(
      "Failed to set value at index `" <> index |> int.to_string <> "`!",
    ),
  )
  data |> Board |> Ok
}

pub fn to_string(board: Board) -> Result(String, String) {
  // data represented as a list of rows, each of which is a list of single characters
  use data_rows: Array(Array(String)) <- result.try(array.sized_chunk(
    board.data |> iv.map(piece.to_string),
    num_rows,
  ))

  data_rows
  // Join each element in a row together with comma separators
  |> iv.map(array.join(_, ", "))
  // Join each row together with newlines
  |> array.join("\n")
  |> Ok
}

fn pos_is_valid(pos: Position) -> Result(Nil, String) {
  let row = pos.row
  let col = pos.col

  use <- bool.guard(
    row >= num_rows,
    Error(
      "Tried to access row index `"
      <> row |> int.to_string
      <> "`, but the board only had `"
      <> num_rows |> int.to_string
      <> "` rows!",
    ),
  )

  use <- bool.guard(
    col >= num_cols,
    Error(
      "Tried to access column index `"
      <> col |> int.to_string
      <> "`, but the board only had `"
      <> num_cols |> int.to_string
      <> "` columns!",
    ),
  )

  Ok(Nil)
}

/// Doesn't take the *entire* fen string: just the first part encoding the board
pub fn from_fen(fen: String) -> Result(Board, String) {
  // Important to start with an empty board, so if we skip some indices,
  // they'll just be filed with null
  let initial = empty().data |> Board

  from_fen_loop(fen, initial, 0, 0)
}

fn from_fen_loop(fen: String, board: Board, col: Int, row: Int) {
  let res = string.pop_grapheme(fen)

  // If we pop the grapheme and get an error, the fen string is over
  use <- bool.guard(res |> result.is_error, Ok(board))

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

      use pos <- result.try(position.new(col, row))
      use new_board <- result.try(set_pos(board, pos, piece))

      from_fen_loop(rest, new_board, col + 1, row)
    }
  }
}
