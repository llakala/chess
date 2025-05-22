import chess/board
import chess/game.{type Game}
import legal/apply
import legal/check

import legal/targets
import position/change
import position/move.{type Move}
import position/position.{type Position}

import piece/square

import utils/text

import gleam/list
import gleam/result
import gleam/string

/// Generates all the legal moves for the current player based on a game state.
pub fn legal_moves(game: Game) -> List(Move) {
  // All the positions containing one of our pieces
  let origins = game.player_positions(game)

  // Get all the moves for each position. If we got an error, panic!
  case list.try_map(origins, moves_from(game, _)) {
    Error(_) ->
      panic as "One of the From positions contained None! Bad logic in getting the list of positions a player is at!"

    // Filter the moves for the ones that don't put us in check after the move
    Ok(nested_moves) ->
      filter_pseudolegal_moves(game, nested_moves |> list.flatten)
  }
}

fn filter_pseudolegal_moves(
  game: Game,
  pseudolegal_moves: List(Move),
) -> List(Move) {
  case check.is_in_check(game) {
    // There's no king of our color on the board - no need to filter at all
    // for moves putting us in check!
    Error(_) -> pseudolegal_moves

    // Currently in check - filter out the moves that would keep us in check
    Ok(#(True, king_pos)) -> {
      // All the pseudolegal moves the king could make from their current
      // position.
      let assert Ok(king_targets) = targets.from_pos(game, king_pos)

      // This will basically be any position in the 1x1 ring around the king,
      // that doesn't contain a friendly piece. We need to find these to see if
      // the king can move directly out of check, or if some other piece can
      // step into the line of fire for it.
      let king_ring_positions =
        king_targets
        |> list.map(fn(target) { target.destination })

      // Positions of enemies directly attacking the king. It might be possible
      // that a friendly piece could take the assassin and stop the threat on
      // the king.
      let attacking_enemy_positions = targets.enemies_to_pos(game, king_pos)

      // Most moves will be illegal, since we're in check. The only legal moves
      // are the ones made by the king, the ones that step into the line of fire
      // FOR the king, or the moves that kill the enemy attacking the king
      let potentially_legal_moves =
        list.filter(pseudolegal_moves, fn(move: Move) {
          list.contains(king_ring_positions, move.change.to)
          || list.contains(attacking_enemy_positions, move.change.to)
        })

      // Apply each of these moves to see if they put us into check. Expensive
      // - so we try to run this on as few moves as possible
      list.filter(potentially_legal_moves, is_move_legal(_, game))
    }

    // Not currently in check - we need to filter out the moves that would put us
    // into check
    Ok(#(False, king_pos)) -> {
      // The positions containing a friendly piece, that might be defending our
      // king
      let checkable_origins = targets.friends_to_pos(game, king_pos)
      // Of course, the king might put itself in check
      let checkable_origins = [king_pos, ..checkable_origins]

      // Most moves won't be by a piece that could actually move us into check.
      // We only need to do extra logic on the moves that could actually take us
      // into check.
      let #(potentially_illegal_moves, legal_moves) =
        list.partition(pseudolegal_moves, fn(move) {
          list.contains(checkable_origins, move.change.from)
        })

      // Apply each of these moves to see if they put us into check. Expensive
      // - so we try to run this on as few moves as possible
      let filtered_moves =
        list.filter(potentially_illegal_moves, is_move_legal(_, game))

      list.append(filtered_moves, legal_moves)
    }
  }
}

fn is_move_legal(move: Move, game: Game) -> Bool {
  case apply.move(game, move) {
    Error(_) -> False
    Ok(new_game) ->
      // If this gives us an error, there was no king of our color on the
      // board. Crazy, I know - it happens in some tests, though.
      case check.is_in_check(new_game) {
        // Ignore the error and return True - meaning the move is safe.
        Error(_) -> True
        // Flip the result we get - we're using `list.filter`, which only
        // keeps the values that were True. So, if we were in check after that
        // move, return False, and filter out the move.
        Ok(#(is_in_check, _)) -> !is_in_check
      }
  }
}

/// Given a game and a position, get all the legal moves that the piece at that
/// position can make. A move wraps a Change, so we can differentiate things like
/// en passant. Returns an error if the position contained None. The data this
/// returns is pretty inefficient, since it stores a bunch of duplicate origin
/// positions. If you want better packed data, use `target.from_pos` - this
/// just wraps its functionality anyways.
pub fn moves_from(game: Game, origin: Position) -> Result(List(Move), String) {
  targets.from_pos(game, origin)
  // Map the result if we got an Ok value
  |> result.map(fn(targets) {
    // Map each target to a move from the origin to the destination, persisting
    // the MoveKind.
    list.map(targets, fn(target) {
      let change = change.Change(origin, target.destination)
      move.Move(change, target.kind)
    })
  })
}

/// Given a list of moves, format/sort the list, and show the origins and
/// destinations of the moves on the board.
pub fn display(moves: List(Move), game: Game) -> String {
  let origins = moves |> list.map(fn(move) { move.change.from })
  let destinations = moves |> list.map(fn(move) { move.change.to })

  // Takes a square and an index, and colors in the origins and destinations of
  // the moves.
  let colorize_square = fn(square, index) {
    let assert Ok(pos) = index |> position.from_index
    let square_str = square |> square.to_string

    case list.contains(origins, pos), list.contains(destinations, pos) {
      // Neither an origin or a destination
      False, False -> square_str

      // Origin, not a destination
      True, False -> square_str |> text.color(text.Cyan)

      // Destination, not an origin
      False, True -> square_str |> text.color(text.Yellow)

      // Both an origin and a destination
      True, True -> square_str |> text.color(text.RGB)
    }
  }
  let moves_output =
    moves
    |> list.sort(move.compare)
    |> list.map(fn(move) { move |> move.to_string })
    |> string.inspect

  let board_output = board.index_format(game.board, colorize_square)

  moves_output <> "\nBoard:\n" <> board_output
}
