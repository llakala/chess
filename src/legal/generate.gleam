import chess/board
import chess/game.{type Game}
import chess/piece.{type Piece, Knight, Pawn}
import chess/position.{type Position}
import chess/sliding.{type Direction, type SlidingPiece}
import chess/square

import gleam/bool
import gleam/int
import gleam/list
import gleam/result
import legal/move.{type Move, Capture, Move}

/// Given a board and a position, get all the legal moves that the piece at that
/// position can make. Returns an error if the position contained None.
pub fn legal_moves(game: Game, pos: Position) -> Result(List(Move), String) {
  let board = game.board
  let square = board.get_pos(board, pos)

  // Returns an error if the position is empty
  use piece <- result.try(square |> square.to_piece)

  case piece {
    Pawn(_) -> legal_pawn_moves(game, pos, piece) |> Ok
    Knight(_) -> legal_knight_moves(game, pos, piece) |> Ok
    _ -> {
      // If this gets an error, there's a logic failure!
      let assert Ok(sliding_piece) = piece |> sliding.new

      legal_sliding_moves(game, pos, sliding_piece)
      |> Ok
    }
  }
}

fn legal_pawn_moves(_game: Game, _pos: Position, _piece: Piece) -> List(Move) {
  []
}

// TODO
fn legal_knight_moves(_game: Game, _pos: Position, _piece: Piece) -> List(Move) {
  []
}

fn legal_sliding_moves(
  game: Game,
  current_pos: Position,
  sliding_piece: SlidingPiece,
) -> List(Move) {
  let board = game.board

  sliding.piece_directions(sliding_piece)
  // For each legal direction cthat our piece can go
  |> list.map(fn(dir) {
    // Distance that our specific kind of piece can go. Built for Knights, who can
    // only go distance 1.
    let piece_distance = sliding.piece_distance(sliding_piece, dir)

    // Store the distance until another piece is found, or we hit a wall. Custom type
    // that can either be a Capture or a NonCapture, so we can use the custom Move
    // constructor for captures.
    let obstructed =
      board.obstructed_distance(board, current_pos, dir, sliding_piece.color)

    // Max distance that our piece can go without obstructions
    let max_distance = int.min(piece_distance, obstructed.distance)

    case obstructed {
      board.Capture(_) -> {
        // Create the capture move first - then call the function to generate the
        // non-captures for all the moves that were of a smaller distance (if they
        // exist)
        let assert Ok(capture_pos) =
          position.from_offset(current_pos, max_distance, dir)
        let capture = Capture(current_pos, capture_pos)

        let non_captures =
          sliding_moves_for_dir(current_pos, max_distance - 1, dir)

        list.prepend(non_captures, capture)
      }
      board.NonCapture(_) -> {
        sliding_moves_for_dir(current_pos, max_distance, dir)
      }
    }
  })
  // Need to flatten because we have multiple lists for each direction internally
  |> list.flatten
}

/// Generates a list of moves from a position and in a direction, up to some maximum
/// distance. This does *not* generate moves with the Capture() record - you're
/// expected to use this to generate your non-captures, and handle captures on your
/// own. This also doesn't care about your piece type - so make sure `max_distance`
/// takes Kings into account, since they can only move by one square!
fn sliding_moves_for_dir(
  current_pos: Position,
  max_distance: Int,
  dir: Direction,
) -> List(Move) {
  // iv.range isn't very safe - given the input `(1,0)`, it doesn't give an error.
  // We have to guard against the case that there are no moves from our current
  // position.
  use <- bool.guard(max_distance == 0, list.new())
  let distances = list.range(1, max_distance)

  // For each legal distance in the current direction
  list.map(distances, fn(dist) {
    // map the direction and the distance to a new position. Returns a result, but
    // if it ever fails, we must've somehow had invalid logic. Insta-fail!
    let assert Ok(new_pos) = position.from_offset(current_pos, dist, dir)

    Move(current_pos, new_pos)
  })
}
