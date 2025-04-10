import chess/board
import chess/color.{Black, White}
import chess/file
import chess/game.{type Game}
import chess/piece.{type Piece, Knight, Pawn}
import chess/position.{type Position}
import chess/rank
import chess/sliding.{
  type Direction, type SlidingPiece, Down, DownLeft, DownRight, Up, UpLeft,
  UpRight,
}
import chess/square
import gleam/option.{type Option}

import gleam/bool
import gleam/int
import gleam/list
import gleam/result
import legal/action.{type Action, Passant}
import legal/change.{Change}

/// Given a board and a position, get all the legal actions that the piece at that
/// position can make. An Action wraps a Change, so we can differentiate things like
/// en passant.Returns an error if the position contained None.
pub fn legal_actions(game: Game, pos: Position) -> Result(List(Action), String) {
  let board = game.board
  let square = board.get_pos(board, pos)

  // Returns an error if the position is empty
  use piece <- result.try(square |> square.to_piece)

  case piece {
    Pawn(_) -> legal_pawn_actions(game, pos, piece) |> Ok
    Knight(_) -> legal_knight_actions(game, pos, piece) |> Ok
    _ -> {
      // If this gets an error, there's a logic failure!
      let assert Ok(sliding_piece) = piece |> sliding.new

      legal_sliding_actions(game, pos, sliding_piece)
      |> Ok
    }
  }
}

fn legal_pawn_actions(game: Game, pos: Position, piece: Piece) -> List(Action) {
  let vertical_actions = pawn_vertical_actions(game, pos, piece)
  let diagonal_actions = pawn_diagonal_actions(game, pos, piece)
  let en_passant = en_passant_actions(game, pos, piece)

  let en_passant_actions = case en_passant {
    option.Some(action) -> action |> list.wrap
    option.None -> []
  }

  list.append(vertical_actions, diagonal_actions)
  |> list.append(en_passant_actions)
}

fn pawn_vertical_actions(
  game: Game,
  pos: Position,
  piece: Piece,
) -> List(Action) {
  let board = game.board

  // A 1-based index, with 0 representing the bottom row
  let rank_index = pos |> position.get_rank |> rank.to_index

  let can_double_move = case piece.color, rank_index {
    Black, 6 -> True
    White, 1 -> True
    _, _ -> False
  }

  let dir = case piece.color {
    Black -> Down
    White -> Up
  }

  // If this failed, we're at the edge and didn't promote into a queen! Means there
  // was bad logic when it came to promotion.
  let assert Ok(new_pos) =
    position.from_offset(distance: 1, position: pos, direction: dir)

  let square = board.get_pos(board, new_pos)

  // Only write the actions to the legal actions list if the space in front of us is
  // empty. We store `was_legal` so we can tell if it's possible to move two away as
  // well
  let #(actions, was_legal) = case square {
    square.Some(_) -> #([], False)
    square.None -> {
      let action = Change(pos, new_pos) |> action.Basic
      #(action |> list.wrap, True)
    }
  }

  // We can only double move if we're on the right rank, and moving one was legal.
  // If not, exit early.
  use <- bool.guard(can_double_move == False || was_legal == False, actions)

  // The `can_double_move` check means we're in no danger of hitting the edge
  // of the board
  let assert Ok(new_pos) =
    position.from_offset(distance: 2, position: pos, direction: dir)

  let square = board.get_pos(board, new_pos)

  case square {
    square.Some(_) -> actions
    square.None -> {
      let double = Change(pos, new_pos) |> action.Basic
      [double, ..actions]
    }
  }
}

fn pawn_diagonal_actions(
  game: Game,
  pos: Position,
  piece: Piece,
) -> List(Action) {
  let board = game.board

  let dirs = case piece.color {
    Black -> [DownLeft, DownRight]
    White -> [UpLeft, UpRight]
  }

  // Rest of the function runs on each direction in the directions
  use dir <- list.filter_map(dirs)

  // If this errors out since we're by an edge, simply don't add it to the list,
  // thanks to `filter_map`
  use pos_in_dir <- result.try(position.from_offset(
    distance: 1,
    position: pos,
    direction: dir,
  ))

  let square = board.get_pos(board, pos_in_dir)

  let is_enemy = case square {
    square.None -> False
    square.Some(other) -> piece.color != other.color
  }

  // Based on the piece found in the direction, and whether that piece is an enemy,
  // decide whether the action is legal. Error means illegal.
  case square, is_enemy {
    // No piece to capture.
    square.None, _ -> Error("")

    // Piece found is of same color, so can't capture.
    square.Some(_), False -> Error("")

    // There's a piece in that direction, and it's an enemy. Legal! Use the Capture
    // constructor, so we can give this action higher priority in evaluation
    square.Some(_), True -> Change(pos, pos_in_dir) |> action.Capture |> Ok
  }
}

fn en_passant_actions(game: Game, pos: Position, piece: Piece) -> Option(Action) {
  // 0-based indices
  let rank = pos |> position.get_rank |> rank.to_index
  let file = pos |> position.get_file |> file.to_index

  let moved_three_spaces = case piece.color {
    White -> rank == 4
    Black -> rank == 5
  }

  // For en passant to happen, an enemy piece had to move two spaces in the previous
  // turn, and we have to be on the correct rank. In tandem, these ensure that en
  // passant is possible in terms of the rows - we just have to check that we're
  // on adjacent columns now.
  case game.passant, moved_three_spaces {
    option.Some(passant_pos), True -> {
      let passant_file = passant_pos |> position.get_rank |> rank.to_index

      // absolute value lets us check that the difference between the two ranks
      // is 1 - note that we already checked that the rank was correct!
      let on_adjacent_file = int.absolute_value(passant_file - file) == 1
      use <- bool.guard(on_adjacent_file == False, option.None)

      // The cool thing about the fen representation of passant is that it stores
      // the position that the passant piece skipped over - meaning we can just move
      // there.
      let passant = Change(pos, passant_pos) |> Passant
      passant |> option.Some
    }

    _, _ -> option.None
  }
}

/// TODO
fn legal_knight_actions(
  _game: Game,
  _position: Position,
  _piece: Piece,
) -> List(Action) {
  []
}

fn legal_sliding_actions(
  game: Game,
  current_pos: Position,
  sliding_piece: SlidingPiece,
) -> List(Action) {
  let board = game.board

  sliding.piece_directions(sliding_piece)
  // For each legal direction cthat our piece can go
  |> list.map(fn(dir) {
    // Distance that our specific kind of piece can go. Built for Knights, who can
    // only go distance 1.
    let piece_distance = sliding.piece_distance(sliding_piece, dir)

    // Store the distance until another piece is found, or we hit a wall. Custom type
    // that can either be a Capture or a NonCapture, so we can mark the action as
    // a capture if needed.
    let obstructed =
      board.obstructed_distance(board, current_pos, dir, sliding_piece.color)

    // Max distance that our piece can go without obstructions
    let max_distance = int.min(piece_distance, obstructed.distance)

    case obstructed {
      board.Capture(_) -> {
        // Create the capture action first - then call the function to generate the
        // non-captures for all the actions that were of a smaller distance (if they
        // exist)
        let assert Ok(capture_pos) =
          position.from_offset(current_pos, max_distance, dir)
        let capture = Change(current_pos, capture_pos) |> action.Capture

        let non_captures =
          sliding_actions_for_dir(current_pos, max_distance - 1, dir)

        list.prepend(non_captures, capture)
      }
      board.NonCapture(_) -> {
        sliding_actions_for_dir(current_pos, max_distance, dir)
      }
    }
  })
  // Need to flatten because we have multiple lists for each direction internally
  |> list.flatten
}

/// Generates a list of actions from a position and in a direction, up to some
/// maximum distance. This does *not* generate actions with the Capture() record -
/// you're expected to use this to generate your non-captures, and handle captures on
/// your own. This also doesn't care about your piece type - so make sure
/// `max_distance` takes Kings into account, since they can only move by one square!
fn sliding_actions_for_dir(
  current_pos: Position,
  max_distance: Int,
  dir: Direction,
) -> List(Action) {
  // iv.range isn't very safe - given the input `(1,0)`, it doesn't give an error.
  // We have to guard against the case that there are no actions from our current
  // position.
  use <- bool.guard(max_distance == 0, list.new())
  let distances = list.range(1, max_distance)

  // For each legal distance in the current direction
  list.map(distances, fn(dist) {
    // map the direction and the distance to a new position. Returns a result, but
    // if it ever fails, we must've somehow had invalid logic. Insta-fail!
    let assert Ok(new_pos) = position.from_offset(current_pos, dist, dir)

    Change(current_pos, new_pos) |> action.Basic
  })
}
