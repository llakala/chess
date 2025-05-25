// My own helper functions for working with `iv` arrays
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string

import utils/text

import chess/constants.{num_cols, num_rows}
import piece/piece.{type Piece}
import piece/square.{type Square}
import position/position.{type Position}

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
  let assert Ok(board) = new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")

  board
}

/// Search the board for some piece. Returns either the first position found
/// containing the given piece, or an error if the piece wasn't found on the
/// board at all.
pub fn search(board: Board, piece: Piece) -> Result(Position, String) {
  case iv.index_of(board.data, square.Some(piece)) {
    Error(Nil) -> Error(piece.to_string(piece) <> " not found on board!")
    Ok(index) -> {
      // This would only fail if the board data was somehow malformed
      let assert Ok(pos) = position.from_index(index)

      pos |> Ok
    }
  }
}

/// Given some board, return a dictionary, mapping some piece to its position(s)
/// on the board. Great for working with a lot of positions without constantly
/// searching!
pub fn piece_positions(board: Board) -> Dict(Piece, List(Position)) {
  let dict = dict.new()

  iv.index_fold(board.data, dict, fn(dict, square, index) {
    let assert Ok(pos) = position.from_index(index)

    case square {
      // Current square is empty - don't add anything to the dict
      square.None -> dict

      square.Some(piece:) -> {
        // The current value for the piece key. Returns a result if the key is
        // currently unset
        case dict.get(dict, piece) {
          // There's no positions currently stored for the piece - create a new
          // value
          Error(_) -> dict |> dict.insert(piece, [pos])

          // The piece already has some other position set - add the new pos to
          // the list!
          Ok(positions) -> dict |> dict.insert(piece, [pos, ..positions])
        }
      }
    }
  })
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
  let index: Int = position.to_index(pos)

  // If this ever fails, it's a logic error, not a user issue
  let assert Ok(square) = board.data |> iv.get(index)

  square
}

/// Getter function to access the board's data, since boards are
/// opaque
pub fn get_data(board: Board) -> Array(Square) {
  board.data
}

/// Set the board's position at the given coordinate
/// Doesn't need to return an error - all of these are
/// opaque types that must be valid to be created
pub fn set_pos(board: Board, pos: Position, square: Square) -> Board {
  // 0 corresponds to top left of the board, in relation to the player
  let index: Int = position.to_index(pos)

  let assert Ok(new_data) =
    board.data
    |> iv.set(index, square)

  Board(new_data)
}

pub fn to_string(board: Board) -> String {
  format(board, square.to_string)
}

/// Highlight a set of positions on the board. If you want to display moves,
/// not positions, you probably want `generate.display`.
pub fn highlight(positions: Set(Position), board: Board) -> String {
  let positions_output =
    positions
    // Yeah, it's linear, but this is for debugging, calm down.
    |> set.to_list
    |> list.sort(position.compare)
    |> list.map(fn(pos) { pos |> position.to_string })
    |> string.inspect

  let board_output =
    index_format(board, fn(square, index) {
      let assert Ok(pos) = index |> position.from_index
      let square_str = square |> square.to_string

      case set.contains(positions, pos) {
        False -> square_str
        True -> square_str |> text.color(text.Yellow)
      }
    })

  positions_output <> "\n" <> board_output
}

/// Get a customizable string representation of the board, that takes a function
/// to apply to each square on the board. This is intended for custom to_string
/// implementations, like specific highlighting. If you just want a string
/// representation of the board, `board.to_string` is what you're looking for.
pub fn format(board: Board, func: fn(Square) -> String) -> String {
  create_formatter(board, list.map(_, func))
}

/// Serves the same function as `board.format` (see its documentation for more
/// info), but with a function that requires the square's position on the board.
/// The index can be turned into a proper Position using `position.from_index`.
pub fn index_format(board: Board, func: fn(Square, Int) -> String) -> String {
  create_formatter(board, list.index_map(_, func))
}

fn create_formatter(
  board: Board,
  func: fn(List(Square)) -> List(String),
) -> String {
  let output =
    board.data
    // Performance doesn't matter here - this is just for debugging and tests.
    |> iv.to_list
    // Apply the passed KIND of function to each position on the board. This
    // will be any function that takes a list of squares and returns a list
    // of strings. I do this so I can keep the same core logic between
    // `board.format` and `board.index_format`. Rather than constantly needing
    // to duplicate logic between the two, they both use this function
    // internally, passing the function THEY recieve into `board.map` and
    // `board.index_map` respectively.
    |> func
    // Take the list of 64 squares, and turn it into a list of 8 lists, each one
    // representing a row - a classic "2d array"
    |> list.sized_chunk(num_rows)
    // Join each element in a row together
    |> list.map(string.join(_, " "))
    // Show which rank each row is before we join the rows together
    |> list.index_map(fn(row, index) {
      // Flips the index so it starts from 8 at the top, not 1
      let rank = int.to_string(constants.num_rows - index)
      text.color(rank, text.Gray) <> " " <> row
    })
    // Join each row together with newlines
    |> string.join("\n")

  output
  // Extra spaces so that the file labels are lined up
  <> "\n  "
  // Label each column, coloring it in gray
  <> text.color("a b c d e f g h", text.Gray)
}

fn from_fen_loop(
  fen: String,
  data: Array(Square),
  col: Int,
  row: Int,
) -> Result(Board, String) {
  // If we pop the grapheme and get an error, the fen string is over - return
  // all the data immediately
  use #(current, rest) <- choose.cases(
    fen |> string.pop_grapheme,
    on_error: fn(_) { data |> Board |> Ok },
  )

  // If we parse num and get Ok, it means we have to skip some of the rest
  // of the row. Handle it early and leave the rest of the code to handle
  // the Error case - where we actually have a piece to handle
  use _ <- choose.cases(int.parse(current), on_ok: fn(skippable) {
    // As long as the amount to skip is sane, keep looping
    case col + skippable > 8 {
      True ->
        Error("Was told to skip more squares than there were left in the row!")
      False -> from_fen_loop(rest, data, col + skippable, row)
    }
  })

  let is_slash = current == "/"
  let row_done = col > 7

  case is_slash, row_done {
    False, True -> Error("Fen forgot to end the row!")

    True, False -> Error("Fen ended too early at col " <> col |> int.to_string)

    // We reached the row separator - move onto the next
    // row, just without the `/`
    True, True -> from_fen_loop(rest, data, 0, row + 1)

    // We can keep going in our current row
    False, False -> {
      // Will fail if the current character isn't a valid fen representation of
      // a piece
      use piece <- result.try(current |> piece.from_fen)

      let square = square.Some(piece)

      let index = row * constants.row_len + col

      let assert Ok(data) = data |> iv.set(index, square)

      from_fen_loop(rest, data, col + 1, row)
    }
  }
}
