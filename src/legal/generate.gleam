import chess/board
import chess/color.{Black, White}
import chess/file
import chess/game.{type Game}
import chess/offset
import chess/piece.{
  type Piece, type PieceKind, Bishop, Knight, Pawn, Piece, Queen, Rook,
}
import chess/position.{type Position}
import chess/rank
import chess/sliding.{
  type Direction, type SlidingPiece, Down, DownLeft, DownRight, Up, UpLeft,
  UpRight,
}
import chess/square

import legal/moves.{type MovesFromPosition}
import legal/target.{
  type Target, Basic, Capture, Passant, PromotionCapture, Target,
}

import utils/choose

import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result

/// For each position, get all the legal moves.
pub fn legal_moves(game: Game) -> List(MovesFromPosition) {
  // All the positions containing one of our pieces - the origins
  let positions = game.player_positions(game)

  // Get the moves at each position, and turn the list of results into a result
  // of lists
  case list.try_map(positions, moves_from(game, _)) {
    Error(_) ->
      panic as "One of the From positions contained None! Bad logic in getting the list of positions a player is at!"

    Ok(moves_for_positions) -> moves_for_positions
  }
}

/// Given a board and a position, get all the places we could move from that
/// position. Returns an error if the origin didn't contain a piece.
pub fn moves_from(
  game: Game,
  origin: Position,
) -> Result(MovesFromPosition, String) {
  let board = game.board
  let square = board.get_pos(board, origin)

  // Returns an error if the position is empty
  use piece <- result.try(square |> square.to_piece)

  let targets = case piece.kind {
    Pawn -> legal_pawn_targets(game, origin, piece)
    Knight -> legal_knight_targets(game, origin, piece)
    _ -> {
      // If this gets an error, there's a logic failure!
      let assert Ok(sliding_piece) = piece |> sliding.new

      legal_sliding_targets(game, origin, sliding_piece)
    }
  }

  moves.MovesFromPosition(origin, targets) |> Ok
}

fn legal_pawn_targets(game: Game, pos: Position, piece: Piece) -> List(Target) {
  let vertical_targets = pawn_vertical_targets(game, pos, piece)
  let diagonal_targets = pawn_diagonal_targets(game, pos, piece)

  let targets = list.append(vertical_targets, diagonal_targets)

  // Unlike the others, there'll only be one en passant target at a time, so it returns
  // an Option. If its return value is non-empty, add it!
  case en_passant_target(game, pos, piece) {
    option.None -> targets
    option.Some(target) -> [target, ..targets]
  }
}

/// Get the kinds of pieces that a pawn can promote into.
fn promotable_piece_kinds() -> List(PieceKind) {
  [Queen, Rook, Bishop, Knight]
}

/// Generates Basic and Promotion targets from the given origin.
fn pawn_vertical_targets(
  game: Game,
  origin: Position,
  piece: Piece,
) -> List(Target) {
  let board = game.board
  let my_color = piece.color

  // A 1-based index, with 0 representing the bottom row
  let rank_index = origin |> position.get_rank |> rank.to_index

  let can_double_move = case piece.color, rank_index {
    Black, 6 -> True
    White, 1 -> True
    _, _ -> False
  }

  let can_promote = case piece.color, rank_index {
    Black, rank -> rank - 1 == 0
    White, rank -> rank + 1 == 7
  }

  let dir = case piece.color {
    Black -> Down
    White -> Up
  }

  // If this failed, we're at the edge and didn't promote into a queen! Means there
  // was bad logic when it came to promotion.
  let assert Ok(pos) =
    position.in_direction(distance: 1, position: origin, direction: dir)

  let square = board.get_pos(board, pos)

  // Only write the targets to the list if the space in front of us is empty. We
  // store `was_legal` so we can tell if it's possible to move two away as well.
  let #(targets, successfully_single_moved) = case square, can_promote {
    square.Some(_), _ -> #([], False)

    // We can move, but we couldn't promote
    square.None, False -> {
      let target = Target(pos, Basic)
      #(target |> list.wrap, True)
    }

    // We can move, and can promote!
    square.None, True -> {
      // Create a target for each potential piece we could promote into
      let targets =
        list.map(promotable_piece_kinds(), fn(kind) {
          let piece = Piece(kind, my_color)
          Target(pos, target.Promotion(piece))
        })

      // Even though we technically did single-move successfully, promoting means
      // there's no way we could double-move, so set it to false
      #(targets, False)
    }
  }

  // We can only double move if we're on the right rank, and moving one was legal.
  // If not, exit early.
  use <- bool.guard(
    can_double_move == False || successfully_single_moved == False,
    targets,
  )

  // The `can_double_move` check means we're in no danger of hitting the edge
  // of the board
  let assert Ok(pos) =
    position.in_direction(distance: 2, position: origin, direction: dir)

  let square = board.get_pos(board, pos)

  case square {
    square.Some(_) -> targets
    square.None -> {
      let double = Target(pos, Basic)
      [double, ..targets]
    }
  }
}

/// Generates Captures and PromotionCaptures
fn pawn_diagonal_targets(
  game: Game,
  origin: Position,
  piece: Piece,
) -> List(Target) {
  let board = game.board

  let dirs = case piece.color {
    Black -> [DownLeft, DownRight]
    White -> [UpLeft, UpRight]
  }

  // A 1-based index, with 0 representing the bottom row
  let rank_index = origin |> position.get_rank |> rank.to_index

  let can_promote = case piece.color, rank_index {
    Black, rank -> rank - 1 == 0
    White, rank -> rank + 1 == 7
  }

  // The rest of this function runs for each direction in the directions. The
  // directions each return a list, since captures create multiple possible moves in
  // a direction (for each piece to be promoted into)
  use dir <- list.flat_map(dirs)

  // If this errors out since we're by an edge, return an empty list representing
  // no legal targets in this direction
  use pos <- choose.cases(
    position.in_direction(distance: 1, position: origin, direction: dir),
    on_error: fn(_) { [] },
  )

  let square = board.get_pos(board, pos)

  let is_enemy = case square {
    square.None -> False
    square.Some(other) -> piece.color != other.color
  }

  // Based on the piece found in the direction, and whether that piece is an enemy,
  // decide whether the target is legal. `can_promote` dictates the type of
  // target(s) to be returned.
  case square, is_enemy, can_promote {
    // No piece to capture - move into the next direction
    square.None, _, _ -> []

    // Piece found is of same color, so can't capture. Move on.
    square.Some(_), False, _ -> []

    // There's a piece in that direction, and it's an enemy (but we can't promote).
    // Use the Capture record, so we can give this target higher priority in evaluation
    square.Some(_), True, False -> Target(pos, Capture) |> list.wrap

    // Promotion!
    square.Some(_), True, True -> {
      let my_color = piece.color
      // Create a target for each piece we could promote into (bishop, rook, queen, or
      // Knight). This is why we use `flat_map` for each direction - some directions
      // will generate multiple targets!
      list.map(promotable_piece_kinds(), fn(kind) {
        let new_piece = Piece(kind, my_color)
        Target(pos, PromotionCapture(new_piece))
      })
    }
  }
}

/// Returns an Option, since there's only one possible en passant target at any given
/// time.
fn en_passant_target(
  game: Game,
  origin: Position,
  piece: Piece,
) -> Option(Target) {
  // 0-based indices
  let rank = origin |> position.get_rank |> rank.to_index
  let origin_file = origin |> position.get_file |> file.to_index

  let moved_three_spaces = case piece.color {
    White -> rank == 4
    Black -> rank == 5
  }

  // For en passant to happen, an enemy piece had to move two spaces in the previous
  // turn, and we have to be on the correct rank. In tandem, these ensure that en
  // passant is possible in terms of the rows - we just have to check that we're
  // on adjacent columns now.
  case game.passant, moved_three_spaces {
    option.Some(pos), True -> {
      let file = pos |> position.get_rank |> rank.to_index

      // absolute value lets us check that the difference between the two ranks
      // is 1 - note that we already checked that the rank was correct!
      let on_adjacent_file = int.absolute_value(file - origin_file) == 1
      use <- bool.guard(on_adjacent_file == False, option.None)

      // The cool thing about the fen representation of passant is that it stores
      // the position that the passant piece skipped over - meaning we can just move
      // there.
      let passant = Target(pos, Passant)
      passant |> option.Some
    }

    _, _ -> option.None
  }
}

fn legal_knight_targets(
  game: Game,
  origin: Position,
  piece: Piece,
) -> List(Target) {
  let board = game.board
  let my_color = piece.color

  // We get all the valid offsets, and apply them - if we get an error, that offset
  // went off the board.
  let potential_positions =
    offset.knight_offsets()
    |> list.filter_map(position.apply_offset(origin, _))

  // Filter out the invalid positions, and turn each valid valid positions into
  // a Target
  list.filter_map(potential_positions, fn(pos) {
    let square = board.get_pos(board, pos)

    case square {
      square.None -> Target(pos, Basic) |> Ok

      // It's an enemy that we can capture!
      square.Some(other_piece) if other_piece.color != my_color ->
        Target(pos, Capture) |> Ok

      // It's a friend - can't go there.
      _ -> Error(Nil)
    }
  })
}

/// Returns BasicMoves and Captures
fn legal_sliding_targets(
  game: Game,
  origin: Position,
  sliding_piece: SlidingPiece,
) -> List(Target) {
  // For each legal direction that our piece can go. Use `flat_map` so a direction
  // can return multiple different targets in that direction
  use dir <- list.flat_map(sliding.piece_directions(sliding_piece))

  let piece_distance = sliding.piece_distance(sliding_piece, dir)

  // Store the distance until another piece is found, or we hit a wall. This
  // lets us mark a target as a Capture if needed.
  let obstructed = game.obstructed_distance(game, origin, dir)

  // Max distance that our piece type can go without obstructions
  let max_distance = int.min(piece_distance, obstructed.distance)

  case obstructed {
    game.Capture(_) -> {
      // Create the capture first - then call the function to generate the
      // non-captures for all the targets of a smaller distance (if they
      // exist)
      let assert Ok(pos) = position.in_direction(origin, max_distance, dir)
      let capture = Target(pos, Capture)

      let non_captures = sliding_targets_for_dir(origin, max_distance - 1, dir)

      list.prepend(non_captures, capture)
    }
    game.NonCapture(_) -> {
      sliding_targets_for_dir(origin, max_distance, dir)
    }
  }
}

/// Generates a list of targets from a position and in a direction, up to some
/// maximum distance. This does *not* generate targets of type Capture  - you're
/// expected to use this to generate your non-captures, and handle captures on
/// your own. This also doesn't care about your piece type - so make sure
/// `max_distance` takes Kings into account, since they can only move by one square!
fn sliding_targets_for_dir(
  current_pos: Position,
  max_distance: Int,
  dir: Direction,
) -> List(Target) {
  // iv.range isn't very safe - given the input `(1,0)`, it doesn't give an error.
  // We have to guard against the case that there are no moves from our current
  // position.
  use <- bool.guard(max_distance == 0, list.new())
  let distances = list.range(1, max_distance)

  // For each legal distance in the current direction
  list.map(distances, fn(dist) {
    // map the direction and the distance to a new position. Returns a result, but
    // if it ever fails, we must've somehow had invalid logic. Insta-fail!
    let assert Ok(new_pos) = position.in_direction(current_pos, dist, dir)

    Target(new_pos, Basic)
  })
}
