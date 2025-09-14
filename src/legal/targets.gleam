import chess/board
import chess/color.{Black, White}
import chess/direction.{
  type Direction, Down, DownLeft, DownRight, Up, UpLeft, UpRight,
}
import chess/game.{type Game}
import chess/move
import chess/offset
import chess/piece.{
  type Piece, type PieceKind, Bishop, Knight, Pawn, Piece, Queen, Rook,
}
import chess/position.{type Position}
import chess/sliding.{type SlidingPiece}
import chess/square
import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/set.{type Set}
import utils/choose

pub type Target {
  Target(destination: Position, kind: move.MoveKind)
}

/// Given a board and a position, get all the legal targets that the piece at that
/// position can make. A target stores the new position, and the kind of move
/// that would be required to move to the given destination. Returns an error
/// if the origin position passed contained None.
pub fn from_pos(game: Game, origin: Position) -> Result(List(Target), String) {
  let board = game.board
  let square = board.get_pos(board, origin)

  // Returns an error if the position is empty
  use piece <- result.try(square |> square.to_piece)

  let targets = case piece.kind {
    Pawn -> legal_pawn_targets(game, origin, piece, False)
    Knight -> legal_knight_targets(game, origin, piece, False)
    _ -> {
      // If this gets an error, there's a logic failure!
      let assert Ok(sliding_piece) = piece |> sliding.new

      legal_sliding_targets(game, origin, sliding_piece, False)
    }
  }

  targets |> Ok
}

/// Alternative to `from_pos`, for when you're doing check detection, and want
/// to see what squares you could capture if there was an enemy there.
pub fn endangered_from_pos(
  game: Game,
  origin: Position,
  piece: Piece,
) -> List(Target) {
  case piece.kind {
    Pawn -> legal_pawn_targets(game, origin, piece, True)
    Knight -> legal_knight_targets(game, origin, piece, True)
    _ -> {
      // If this gets an error, there's a logic failure!
      let assert Ok(sliding_piece) = piece |> sliding.new

      legal_sliding_targets(game, origin, sliding_piece, True)
    }
  }
}

/// Given some list of targets, return the set of positions that can be reached
/// via those targets.
pub fn get_destinations(targets: List(Target)) -> Set(Position) {
  targets
  |> set.from_list
  |> set.map(fn(target) { target.destination })
}

/// Given some position, return all the positions a queen could move to. This
/// includes empty squares and enemy positions.
fn queen_viewable_positions(game: Game, pos: Position) -> Set(Position) {
  let color = game.color

  // Tells us the direct lines of sight
  let queen = sliding.Queen(color)

  // We're going backwards - all the queen targets from our position
  // will tell us all the positions we could attack - which then tells us
  let queen_targets = legal_sliding_targets(game, pos, queen, False)

  queen_targets |> get_destinations
}

/// Given a position, return all the positions a knight could go from there.
fn knight_viewable_positions(game: Game, pos: Position) -> Set(Position) {
  let color = game.color
  let knight = piece.Piece(piece.Knight, color)
  let knight_targets = legal_knight_targets(game, pos, knight, False)

  knight_targets |> get_destinations
}

/// Given some position, return all the positions that contain an enemy piece,
/// who might be attacking our current position.
pub fn enemies_to_pos(game: Game, pos: Position) -> Set(Position) {
  let queen_positions = queen_viewable_positions(game, pos)
  let knight_positions = knight_viewable_positions(game, pos)
  let positions = set.union(knight_positions, queen_positions)

  positions
  // Filter out empty squares
  |> set.filter(fn(pos) {
    let square = board.get_pos(game.board, pos)
    case square {
      square.None -> False
      square.Some(_) -> True
    }
  })
}

/// Given some position, return all the positions that contain a friendly piece,
/// who might be protecting our current square.
pub fn friends_to_pos(game: Game, pos: Position) -> Set(Position) {
  // We need to flip the color, since there are some deep function dependencies
  // of `queen_viewable_positions` that use the game's color - and refactoring
  // all of those isn't feasible at this time. We don't get knight positions,
  // since a knight couldn't defend us - only direct lines can.
  let positions = queen_viewable_positions(game |> game.flip, pos)

  positions
  |> set.filter(fn(pos) {
    let square = board.get_pos(game.board, pos)
    case square {
      square.None -> False

      square.Some(_) -> True
    }
  })
}

/// Given some position, return all the positions that we can "see" - the ones
/// that could be a line of fire for an attack.
pub fn empty_to_pos(game: Game, pos: Position) -> Set(Position) {
  // I don't *THINK* a knight position can be a line of fire. I might be wrong,
  // though!
  let positions = queen_viewable_positions(game, pos)

  positions
  |> set.filter(fn(pos) {
    let square = board.get_pos(game.board, pos)
    case square {
      square.Some(_) -> False
      square.None -> True
    }
  })
}

fn legal_pawn_targets(
  game: Game,
  pos: Position,
  piece: Piece,
  captures_only: Bool,
) -> List(Target) {
  // If we're doing king stuff, these moves don't count, since they're not
  // captures, so we can move there as the king.
  let vertical_targets = case captures_only {
    False -> pawn_vertical_targets(game, pos, piece)
    True -> []
  }

  let diagonal_targets = pawn_diagonal_targets(game, pos, piece, captures_only)

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

/// Generates Basic and Promotion targets.
fn pawn_vertical_targets(
  game: Game,
  pos: Position,
  piece: Piece,
) -> List(Target) {
  let board = game.board
  let my_color = piece.color

  // A 0-based index, with 0 representing the bottom row
  let row_index = pos |> position.rank_index

  let can_double_move = case piece.color, row_index {
    Black, 6 -> True
    White, 1 -> True
    _, _ -> False
  }

  let can_promote = case piece.color, row_index {
    Black, rank -> rank - 1 == 0
    White, rank -> rank + 1 == 7
  }

  let dir = case piece.color {
    Black -> Down
    White -> Up
  }

  // If this failed, we're at the edge and didn't promote into a queen! Means there
  // was bad logic when it came to promotion.
  let assert Ok(new_pos) =
    position.in_direction(distance: 1, position: pos, direction: dir)

  let square = board.get_pos(board, new_pos)

  // Only write the target to the list if the space in front of us is empty.
  // We store `was_legal` so we can tell if it's possible to move two away as
  // well
  let #(targets, successfully_single_moved) = case square, can_promote {
    square.Some(_), _ -> #([], False)

    // We can move, but we couldn't promote
    square.None, False -> {
      let target = Target(new_pos, move.Basic)
      #(target |> list.wrap, True)
    }

    // We can move, and can promote!
    square.None, True -> {
      // Create a target for each potential piece we could promote into
      let targets =
        list.map(promotable_piece_kinds(), fn(kind) {
          let piece = Piece(kind, my_color)
          Target(new_pos, move.Promotion(piece))
        })

      // Even though we technically could single-move successfully, promoting means
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
  let assert Ok(new_pos) =
    position.in_direction(distance: 2, position: pos, direction: dir)

  let square = board.get_pos(board, new_pos)

  case square {
    square.Some(_) -> targets
    square.None -> {
      let double = Target(new_pos, move.Basic)
      [double, ..targets]
    }
  }
}

/// Generates Captures and PromotionCaptures
fn pawn_diagonal_targets(
  game: Game,
  old_pos: Position,
  piece: Piece,
  captures_only: Bool,
) -> List(Target) {
  let board = game.board

  let dirs = case piece.color {
    Black -> [DownLeft, DownRight]
    White -> [UpLeft, UpRight]
  }

  // A 0-based index, with 0 representing the bottom row
  let row = old_pos |> position.rank_index

  let can_promote = case piece.color, row {
    Black, rank -> rank - 1 == 0
    White, rank -> rank + 1 == 7
  }

  // The rest of this function runs for each direction in the directions. The
  // directions each return a list, since captures create multiple possible
  // targets in a direction (for each piece to be promoted into)
  use dir <- list.flat_map(dirs)

  // If this errors out since we're by an edge, return an empty list representing
  // no legal targets in this direction
  use new_pos <- choose.cases(
    position.in_direction(distance: 1, position: old_pos, direction: dir),
    on_error: fn(_) { [] },
  )

  let square = board.get_pos(board, new_pos)

  let is_enemy = case captures_only, square {
    // If we only want to see what a pawn COULD capture if there was a diagonal
    // enemy, we'll make this always true - representing that empty and friendly
    // diagonals will be counted too.
    True, _ -> True

    False, square.None -> False
    False, square.Some(other) -> piece.color != other.color
  }

  // Based on the piece found in the direction, and whether that piece is an enemy,
  // decide whether the target is legal. `can_promote` dictates the type of
  // target(s) to be returned.
  case is_enemy, can_promote {
    // Piece found is of same color, or no piece at desired square, so can't
    // capture. Move on.
    False, _ -> []

    // There's a piece in that direction, and it's an enemy (but we can't promote).
    // Use the Capture type, so we can give this target higher priority in
    // evaluation.
    True, False -> {
      Target(new_pos, move.Capture) |> list.wrap
    }

    // Promotion!
    True, True -> {
      let my_color = piece.color
      // Create a target for each piece we could promote into (bishop, rook, queen, or
      // Knight). This is why we use `flat_map` for each direction - some directions
      // will generate multiple targets!
      list.map(promotable_piece_kinds(), fn(kind) {
        let new_piece = Piece(kind, my_color)
        Target(new_pos, move.PromotionCapture(new_piece))
      })
    }
  }
}

/// Returns an Option, since there's only one possible en passant target at any given
/// time.
fn en_passant_target(game: Game, pos: Position, piece: Piece) -> Option(Target) {
  let row = position.rank_index_one_based(pos)
  let col = position.file_index_one_based(pos)

  let moved_three_spaces = case piece.color {
    White -> row == 5
    Black -> row == 4
  }

  // For en passant to happen, an enemy piece had to move two spaces in the previous
  // turn, and we have to be on the correct rank. In tandem, these ensure that en
  // passant is possible in terms of the rows - we just have to check that we're
  // on adjacent columns now.
  case game.passant, moved_three_spaces {
    option.Some(passant_pos), True -> {
      let passant_file = position.file_index_one_based(passant_pos)

      // absolute value lets us check that the difference between the two files
      // is 1 - note that we already checked that the rank was correct!
      let on_adjacent_file = int.absolute_value(passant_file - col) == 1

      use <- bool.guard(on_adjacent_file == False, option.None)

      // The cool thing about the fen representation of passant is that it stores
      // the position that the passant piece skipped over - meaning we can just
      // make that our destination.
      let passant = Target(passant_pos, move.Passant)
      passant |> option.Some
    }

    _, _ -> option.None
  }
}

fn legal_knight_targets(
  game: Game,
  current_pos: Position,
  piece: Piece,
  captures_only: Bool,
) -> List(Target) {
  let board = game.board
  let my_color = piece.color
  // We get all the valid offsets, and apply them - if we get an error, that offset
  // went off the board.
  let potential_positions =
    offset.knight_offsets()
    |> list.filter_map(position.apply_offset(current_pos, _))

  // Filter out the invalid positions, and turn the valid positions into Targets
  list.filter_map(potential_positions, fn(new_pos) {
    case captures_only, board.get_pos(board, new_pos) {
      // If we only want to see the squares this knight COULD attack, we mark
      // any position the knight can see as a capture. The MoveKind will be
      // wrong here sometimes, but we'll only be using this for hypotheticals,
      // so it doesn't really matter.
      True, _ -> Target(new_pos, move.Basic) |> Ok

      // If the square we want to move to is empty, it's a basic target! Exit early.
      False, square.None -> Target(new_pos, move.Basic) |> Ok

      // It's an enemy that we can capture!
      False, square.Some(other_piece) if other_piece.color != my_color ->
        Target(new_pos, move.Capture) |> Ok

      // It's a friend - can't go there.
      False, _ -> Error(Nil)
    }
  })
}

/// Returns Basic and Capture targets
fn legal_sliding_targets(
  game: Game,
  current_pos: Position,
  sliding_piece: SlidingPiece,
  captures_only: Bool,
) -> List(Target) {
  // For each legal direction that our piece can go. Use `flat_map` so a direction
  // can return multiple different targets in that direction
  use dir <- list.flat_map(sliding.piece_directions(sliding_piece))

  let piece_distance = sliding.piece_distance(sliding_piece, dir)

  // Store the distance until another piece is found, or we hit a wall. Custom type
  // that can either be a Capture or a NonCapture, so we can mark the target as
  // a capture if needed.
  let obstructed =
    game.obstructed_distance(
      game,
      current_pos,
      dir,
      sliding_piece.color,
      captures_only,
    )

  // Max distance that our piece can go without obstructions
  let max_distance = int.min(piece_distance, obstructed.distance)

  case obstructed {
    game.Capture(_) -> {
      // Create the capture target first - then call the function to generate the
      // non-captures for all the targets that were of a smaller distance (if they
      // exist)
      let assert Ok(capture_pos) =
        position.in_direction(current_pos, max_distance, dir)
      let capture = Target(capture_pos, move.Capture)

      let non_captures =
        sliding_targets_for_dir(current_pos, max_distance - 1, dir)

      list.prepend(non_captures, capture)
    }
    game.NonCapture(_) -> {
      sliding_targets_for_dir(current_pos, max_distance, dir)
    }
  }
}

/// Generates a list of targets from a position and in a direction, up to some
/// maximum distance. This does *not* generate targets of type Capture - you're
/// expected to use this to generate your non-captures, and handle captures on
/// your own. This also doesn't care about your piece type - so make sure
/// `max_distance` takes Kings into account, since they can only move by one square!
fn sliding_targets_for_dir(
  current_pos: Position,
  max_distance: Int,
  dir: Direction,
) -> List(Target) {
  // iv.range isn't very safe - given the input `(1,0)`, it doesn't give an error.
  // We have to guard against the case that there are no targets from our current
  // position.
  use <- bool.guard(max_distance == 0, list.new())
  let distances = list.range(1, max_distance)

  // For each legal distance in the current direction
  list.map(distances, fn(dist) {
    // map the direction and the distance to a new position. Returns a result, but
    // if it ever fails, we must've somehow had invalid logic. Insta-fail!
    let assert Ok(new_pos) = position.in_direction(current_pos, dist, dir)

    Target(new_pos, move.Basic)
  })
}
