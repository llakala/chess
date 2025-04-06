import chess/board.{type Board}
import chess/piece.{type Piece, Knight, Pawn}
import chess/position.{type Position}
import chess/sliding.{type SlidingPiece}
import chess/square
import gleam/bool
import gleam/list

import gleam/int
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
  use board <- result.try(board.set_pos(board, from, square.None))

  // And move it to its new position
  use board <- result.try(board.set_pos(board, to, piece))

  Ok(board)
}

/// Given a board and a position, get all the legal moves that the piece at that
/// position can make. Returns an error if the position contained None.
pub fn legal_moves(board: Board, pos: Position) -> Result(List(Move), String) {
  let square = board.get_pos(board, pos)
  // Returns an error if the position is empty
  use piece <- result.try(square |> square.to_piece)
  case piece {
    Pawn(_) -> legal_pawn_moves(board, pos, piece)
    Knight(_) -> legal_knight_moves(board, pos, piece)
    _ -> {
      // If this gets an error, there's a logic failure!
      let assert Ok(sliding_piece) = piece |> sliding.new

      legal_sliding_moves(board, pos, sliding_piece)
    }
  }
}

// TODO
fn legal_pawn_moves(
  _board: Board,
  _current_pos: Position,
  _piece: Piece,
) -> Result(List(Move), String) {
  Error("Unimplemented!")
}

// TODO
fn legal_knight_moves(
  _board: Board,
  _current_pos: Position,
  _piece: Piece,
) -> Result(List(Move), String) {
  Error("Unimplemented!")
}

fn legal_sliding_moves(
  board: Board,
  current_pos: Position,
  sliding_piece: SlidingPiece,
) -> Result(List(Move), String) {
  sliding.piece_directions(sliding_piece)
  // For each legal direction cthat our piece can go
  |> list.map(fn(dir) {
    // Distance until another piece is found, or we hit a wall. Will be inclusive
    // if the other piece is an enemy, so we have the chance to capture it.
    let obstructed_distance =
      board.obstructed_distance(board, current_pos, dir, sliding_piece.color)

    let piece_distance = sliding.piece_distance(sliding_piece, dir)

    // Max distance that our piece can go without obstructions
    let max_distance = int.min(piece_distance, obstructed_distance)

    // iv.range isn't very safe - given the input `(1,0)`, it doesn't give an error.
    // We have to guard against the case that there are no moves from our current
    // position.
    use <- bool.guard(max_distance == 0, list.new())
    let distances = list.range(1, max_distance)

    // For each legal distance in the current direction
    list.map(distances, fn(dist) {
      // map the direction and the distance to a new position. returns a result, but
      // if it ever fails, we must've somehow had invalid logic. Insta-fail!
      let assert Ok(new_pos) = position.from_offset(current_pos, dist, dir)

      // We don't use the constructor, since all it currently does is check that
      // current_pos isn't None, and we've already checked that.
      Move(current_pos, new_pos)
    })
  })
  // Need to flatten because we have multiple lists for each direction internally
  |> list.flatten
  |> Ok
}
