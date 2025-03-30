import chess/board.{type Board}
import chess/position.{type Position}
import chess/sliding
import chess/square
import gleam/bool

import gleam/int
import gleam/result

import iv.{type Array}

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
  let from_str = move.current |> position.to_algebraic
  let to_str = move.new |> position.to_algebraic

  from_str <> " -> " <> to_str
}

/// Constructor for the Move type. The board is only passed in to validate that a move is actually valid for that board
pub fn new(
  board: board.Board,
  from: Position,
  to: Position,
) -> Result(Move, String) {
  use square <- result.try(board.get_pos(board, from))

  // TODO: actually take the piece outputted and pipe it into functions
  use _ <- result.try(
    square
    |> square.to_piece
    |> result.replace_error(
      "Tried to move piece at position "
      <> from |> position.to_algebraic
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

  use piece <- result.try(board.get_pos(board, from))

  // Delete the piece from its current position
  use board <- result.try(board.set_pos(board, from, square.None))

  // And move it to its new position
  use board <- result.try(board.set_pos(board, to, piece))

  Ok(board)
}

// TODO: make private, and have a public `legal_moves` function that calls this
// with sliding pieces.

/// Given a board and a position, get all the legal moves that a sliding piece can
/// make from that position. Returns an error if the position contained None, or a
/// non-sliding piece
pub fn legal_sliding_moves(
  board: Board,
  current_pos: Position,
) -> Result(Array(Move), String) {
  // Get the square stored at the position, then the sliding piece at the square.
  // Returns an error if the position contained None or a non-sliding piece.
  use square <- result.try(board |> board.get_pos(current_pos))
  use piece <- result.try(square |> sliding.from_square)

  sliding.piece_directions(piece)
  // For each legal direction cthat our piece can go
  |> iv.map(fn(dir) {
    // Distance until another piece is found, or we hit a wall. Will be inclusive
    // if the other piece is an enemy, so we have the chance to capture it.
    let obstructed_distance =
      board.obstructed_distance(board, current_pos, dir, piece.color)

    let piece_distance = sliding.piece_distance(piece, dir)

    // Max distance that our piece can go without obstructions
    let max_distance = int.min(piece_distance, obstructed_distance)

    // iv.range isn't very safe - given the input `(1,0)`, it doesn't give an error.
    // We have to guard against the case that there are no moves from our current
    // position.
    use <- bool.guard(max_distance == 0, iv.new())
    let distances = iv.range(1, max_distance)

    // For each legal distance in the current direction
    iv.map(distances, fn(dist) {
      // map the direction and the distance to a new position. returns a result, but
      // if it ever fails, we must've somehow had invalid logic. Insta-fail!
      let assert Ok(new_pos) = position.from_offset(current_pos, dist, dir)

      // We don't use the constructor, since all it currently does is check that
      // current_pos isn't None, and we've already checked that.
      Move(current_pos, new_pos)
    })
  })
  // Need to flatten because we have multiple arrays for each direction internally
  |> iv.flatten
  |> Ok
}
