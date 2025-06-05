import chess/board
import chess/game.{type Game}
import gleam/set
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
  let origins =
    game.player_positions(game)
    |> set.to_list

  // Get all the moves for each position. If we got an error, panic! We use the
  // internal `pseudolegal_moves_from` function, since we want to filter all
  // illegal moves in one step, not multiple.
  case list.try_map(origins, pseudolegal_moves_from(game, _)) {
    Error(_) ->
      panic as "One of the From positions contained None! Bad logic in getting the list of positions a player is at!"

    // Filter the moves for the ones that don't put us in check after the move
    Ok(nested_moves) ->
      nested_moves |> list.flatten |> check.filter_pseudolegal_moves(game)
  }
}

/// Given a game and a position, get all the legal moves that the piece at that
/// position can make. A move wraps a Change, so we can differentiate things like
/// en passant. Returns an error if the position contained None. The data this
/// returns is pretty inefficient, since it stores a bunch of duplicate origin
/// positions. If you want better packed data, use `target.from_pos` - this
/// just wraps its functionality anyways.
pub fn moves_from(game: Game, origin: Position) -> Result(List(Move), String) {
  pseudolegal_moves_from(game, origin)
  |> result.map(check.filter_pseudolegal_moves(_, game))
}

/// This is the internal function that doesn't filter pseudolegal moves further.
/// This lets us filter all illegal moves in one step within `legal_moves`,
/// rather than having it done in multiple steps.
fn pseudolegal_moves_from(
  game: Game,
  origin: Position,
) -> Result(List(Move), String) {
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
      True, True -> square_str |> text.color(text.Red)
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
