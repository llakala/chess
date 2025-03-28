import chess/array
import chess/board.{type Board}
import chess/color.{Black, White}
import chess/piece.{type Piece, Knight, Pawn}
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
pub fn move(board: Board, move: Move) -> Result(Board, String) {
  let from = move |> from()
  let to = move |> to()

  use piece <- result.try(board.get_pos(board, from))

  // Delete the piece from its current position
  use board <- result.try(board.set_pos(board, from, square.None))

  // And move it to its new position
  use board <- result.try(board.set_pos(board, to, piece))

  Ok(board)
}

pub fn sliding_legal_moves(
  board: Board,
  old_pos: Position,
  piece: Piece,
) -> Result(Array(Move), String) {
  use <- bool.guard(
    piece == Knight(Black)
      || piece == Knight(White)
      || piece == Pawn(Black)
      || piece == Pawn(White),
    Error(
      "Pawns and knights aren't sliding pieces, so their legal moves have to be found differently!",
    ),
  )

  let color = piece.color

  // Get a list of directions that a piece is allowed to go.
  todo
  |> iv.map(fn(dir) {
    // Distance until another piece is found, or we hit a wall. Will be inclusive
    // if the other piece is an enemy, so we have the chance to capture it.
    let obstructed_distance =
      board.obstructed_distance(board, old_pos, dir, color)

    // Distance that a sliding piece can go. Returns an error if the piece doesn't
    // slide (pawn or knight), but we've already handled that above.
    let assert Ok(piece_distance) = sliding.piece_distance(piece, dir)

    let max_distance = int.min(piece_distance, obstructed_distance)

    let distances = iv.range(1, max_distance)

    // from_offset returns a result, but if it ever fails, we must've somehow had
    // invalid logic. Insta-fail. Then create a new move using the type constructor.
    // The constructor can fail, so we end up with a Result.
    iv.map(distances, fn(dist) {
      let assert Ok(new_pos) = position.from_offset(old_pos, dist, dir)

      // This can fail if old_pos points to None. We technically haven't checked
      // this yet - although it might be better to check this earlier since it would
      // apply for all moves. Might be nice for if `new()` adds more legality checks
      // in the future.
      new(board, old_pos, new_pos)
    })
  })
  // Need to flatten because we have multiple arrays for each direction internally
  |> iv.flatten
  // If any of the calls to `new()` returned an error, something went wrong.
  |> array.all_ok
}
