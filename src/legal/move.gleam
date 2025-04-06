import chess/board.{type Board}
import chess/position.{type Position}
import chess/square.{None}

import gleam/result

pub opaque type Move {
  Move(current: Position, new: Position)
}

/// Getter for accessing the position to be moved from
pub fn from(move: Move) -> Position {
  move.current
}

/// Getter for accessing the position to be moved to
pub fn to(move: Move) -> Position {
  move.new
}

pub fn to_string(move: Move) -> String {
  let from_str = move.current |> position.to_string
  let to_str = move.new |> position.to_string

  from_str <> " -> " <> to_str
}

/// Constructor for the Move type. The board is only passed in to validate that a move is actually valid for that board
pub fn new(
  board: board.Board,
  from: Position,
  to: Position,
) -> Result(Move, String) {
  let square = board.get_pos(board, from)

  // TODO: actually take the piece outputted and pipe it into functions
  use _ <- result.try(
    square
    |> square.to_piece
    |> result.replace_error(
      "Tried to move piece at position "
      <> from |> position.to_string
      <> ", but there was no piece there!",
    ),
  )

  Move(from, to) |> Ok
}

/// Simple move function that moves a piece to another place on the board,
/// capturing if another piece is at the target square. This function
/// doesn't encode any logic about legal moves - you can make a piece go
/// anywhere with this as it stands.
/// I would put this in the `board` class, but then I get circular
/// dependency issues.
pub fn apply(board: Board, move: Move) -> Result(Board, String) {
  let from = move |> from()
  let to = move |> to()

  let piece = board.get_pos(board, from)

  // Delete the piece from its current position
  use board <- result.try(board.set_pos(board, from, None))

  // And move it to its new position
  use board <- result.try(board.set_pos(board, to, piece))

  Ok(board)
}
