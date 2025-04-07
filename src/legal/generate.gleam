import chess/board
import chess/game.{type Game}
import chess/piece.{type Piece, Knight, Pawn}
import chess/position.{type Position}
import chess/sliding.{type SlidingPiece}
import chess/square

import gleam/bool
import gleam/int
import gleam/list
import gleam/result
import legal/move.{type Move, Move}

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

      Move(current_pos, new_pos)
    })
  })
  // Need to flatten because we have multiple lists for each direction internally
  |> list.flatten
}
