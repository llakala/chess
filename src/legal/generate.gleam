import chess/board
import chess/color.{type Color, Black, White}
import chess/file
import chess/game.{type Game}
import chess/offset
import chess/piece.{type Piece, Bishop, Knight, Pawn, Queen, Rook}
import chess/position.{type Position}
import chess/rank
import chess/sliding.{
  type Direction, type SlidingPiece, Down, DownLeft, DownRight, Up, UpLeft,
  UpRight,
}
import chess/square
import gleam/option.{type Option}
import gleam/string
import utils/choose

import gleam/bool
import gleam/int
import gleam/list
import gleam/result
import legal/change.{Change}
import legal/move.{type Move, Passant}

/// Given a board and a position, get all the legal moves that the piece at that
/// position can make. An move wraps a Change, so we can differentiate things like
/// en passant.Returns an error if the position contained None.
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

// Given a list of generated moves, display them for testing - first sorted, then
// as their string representations.
pub fn display(moves: List(Move)) -> String {
  moves
  |> list.sort(move.compare)
  |> list.map(fn(move) { move |> move.to_string })
  |> string.inspect
}

fn legal_pawn_moves(game: Game, pos: Position, piece: Piece) -> List(Move) {
  let vertical_moves = pawn_vertical_moves(game, pos, piece)
  let diagonal_moves = pawn_diagonal_moves(game, pos, piece)

  let moves = list.append(vertical_moves, diagonal_moves)

  // Unlike the others, there'll only be one en passant move at a time, so it returns
  // an Option. If its return value is non-empty, add it!
  case en_passant_move(game, pos, piece) {
    option.None -> moves
    option.Some(move) -> [move, ..moves]
  }
}

/// Get the pieces that a pawn can promote into. Takes a Color, so the pieces are of
/// the same color as the to-be-promoted pawn. Hopefully I'll eventually refactor the
/// Piece type to contain a PieceType and a Color, so this kind of thing isn't always
/// necessary.
fn promotable_pieces(color: Color) {
  [Queen(color), Rook(color), Bishop(color), Knight(color)]
}

/// Generates BasicMoves and Promotions.
fn pawn_vertical_moves(game: Game, pos: Position, piece: Piece) -> List(Move) {
  let board = game.board
  let my_color = piece.color

  // A 1-based index, with 0 representing the bottom row
  let rank_index = pos |> position.get_rank |> rank.to_index

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
  let assert Ok(new_pos) =
    position.in_direction(distance: 1, position: pos, direction: dir)

  let square = board.get_pos(board, new_pos)

  // Only write the moves to the legal moves list if the space in front of us is
  // empty. We store `was_legal` so we can tell if it's possible to move two away as
  // well
  let #(moves, successfully_single_moved) = case square, can_promote {
    square.Some(_), _ -> #([], False)

    // We can move, but we couldn't promote
    square.None, False -> {
      let move = Change(pos, new_pos) |> move.Basic
      #(move |> list.wrap, True)
    }

    // We can move, and can promote!
    square.None, True -> {
      // Create a move for each potential piece we could promote into
      let moves =
        list.map(promotable_pieces(my_color), fn(piece) {
          Change(pos, new_pos) |> move.Promotion(piece)
        })

      // Even though we technically did single-move successfully, promoting means
      // there's no way we could double-move, so set it to false
      #(moves, False)
    }
  }

  // We can only double move if we're on the right rank, and moving one was legal.
  // If not, exit early.
  use <- bool.guard(
    can_double_move == False || successfully_single_moved == False,
    moves,
  )

  // The `can_double_move` check means we're in no danger of hitting the edge
  // of the board
  let assert Ok(new_pos) =
    position.in_direction(distance: 2, position: pos, direction: dir)

  let square = board.get_pos(board, new_pos)

  case square {
    square.Some(_) -> moves
    square.None -> {
      let double = Change(pos, new_pos) |> move.Basic
      [double, ..moves]
    }
  }
}

/// Generates Captures and PromotionCaptures
fn pawn_diagonal_moves(
  game: Game,
  old_pos: Position,
  piece: Piece,
) -> List(Move) {
  let board = game.board

  let dirs = case piece.color {
    Black -> [DownLeft, DownRight]
    White -> [UpLeft, UpRight]
  }

  // A 1-based index, with 0 representing the bottom row
  let rank_index = old_pos |> position.get_rank |> rank.to_index

  let can_promote = case piece.color, rank_index {
    Black, rank -> rank - 1 == 0
    White, rank -> rank + 1 == 7
  }

  // The rest of this function runs for each direction in the directions. The
  // directions each return a list, since captures create multiple possible moves in
  // a direction (for each piece to be promoted into)
  use dir <- list.flat_map(dirs)

  // If this errors out since we're by an edge, return an empty list representing
  // no legal movesi n this direction
  use new_pos <- choose.cases(
    position.in_direction(distance: 1, position: old_pos, direction: dir),
    on_error: fn(_) { [] },
  )

  let square = board.get_pos(board, new_pos)

  let is_enemy = case square {
    square.None -> False
    square.Some(other) -> piece.color != other.color
  }

  // Based on the piece found in the direction, and whether that piece is an enemy,
  // decide whether the move is legal. `can_promote` dictates the type of move(s) to
  // be returned.
  case square, is_enemy, can_promote {
    // No piece to capture - move into the next direction
    square.None, _, _ -> []

    // Piece found is of same color, so can't capture. Move on.
    square.Some(_), False, _ -> []

    // There's a piece in that direction, and it's an enemy (but we can't promote).
    // Use the Capture constructor, so we can give this move higher priority in evaluation
    square.Some(_), True, False ->
      Change(old_pos, new_pos) |> move.Capture |> list.wrap

    // Promotion!
    square.Some(_), True, True -> {
      let my_color = piece.color
      // Create a move for each piece we could promote into (bishop, rook, queen, or
      // Knight). This is why we use `flat_map` for each direction - some directions
      // will generate multiple moves!
      list.map(promotable_pieces(my_color), fn(new_piece) {
        Change(old_pos, new_pos) |> move.PromotionCapture(new_piece)
      })
    }
  }
}

/// Returns an Option, since there's only one possible en passant move at any given
/// time.
fn en_passant_move(game: Game, pos: Position, piece: Piece) -> Option(Move) {
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
fn legal_knight_moves(
  game: Game,
  current_pos: Position,
  piece: Piece,
) -> List(Move) {
  let board = game.board
  let my_color = piece.color
  // We get all the valid offsets, and apply them - if we get an error, that offset
  // went off the board.
  let potential_positions =
    offset.knight_offsets()
    |> list.filter_map(position.apply_offset(current_pos, _))

  // Filter out the invalid positions, and turn the valid positions into Moves
  list.filter_map(potential_positions, fn(new_pos) {
    let new_square = board.get_pos(board, new_pos)

    case new_square {
      // If the square we want to move to is empty, it's a basic move! Exit early.
      square.None -> change.Change(current_pos, new_pos) |> move.Basic |> Ok

      // It's an enemy that we can capture!
      square.Some(other_piece) if other_piece.color != my_color ->
        change.Change(current_pos, new_pos) |> move.Capture |> Ok

      // It's a friend - can't go there.
      _ -> Error(Nil)
    }
  })
}

/// Returns BasicMoves and Captures
fn legal_sliding_moves(
  game: Game,
  current_pos: Position,
  sliding_piece: SlidingPiece,
) -> List(Move) {
  let board = game.board

  // For each legal direction that our piece can go. Use `flat_map` so a direction
  // can return multiple different legal moves in that direction
  use dir <- list.flat_map(sliding.piece_directions(sliding_piece))

  let piece_distance = sliding.piece_distance(sliding_piece, dir)

  // Store the distance until another piece is found, or we hit a wall. Custom type
  // that can either be a Capture or a NonCapture, so we can mark the move as
  // a capture if needed.
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
        position.in_direction(current_pos, max_distance, dir)
      let capture = Change(current_pos, capture_pos) |> move.Capture

      let non_captures =
        sliding_moves_for_dir(current_pos, max_distance - 1, dir)

      list.prepend(non_captures, capture)
    }
    board.NonCapture(_) -> {
      sliding_moves_for_dir(current_pos, max_distance, dir)
    }
  }
}

/// Generates a list of moves from a position and in a direction, up to some
/// maximum distance. This does *not* generate moves with the Capture() record -
/// you're expected to use this to generate your non-captures, and handle captures on
/// your own. This also doesn't care about your piece type - so make sure
/// `max_distance` takes Kings into account, since they can only move by one square!
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
    let assert Ok(new_pos) = position.in_direction(current_pos, dist, dir)

    Change(current_pos, new_pos) |> move.Basic
  })
}
