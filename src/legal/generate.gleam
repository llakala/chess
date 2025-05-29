import chess/board
import chess/game.{type Game}
import gleam/dict
import gleam/set
import legal/check
import legal/tarmap.{type TarmapCollection}

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
  let tarmaps = legal_tarmaps(game)
  let pseudolegal_moves = tarmap.collection_to_moves(tarmaps)

  check.filter_pseudolegal_moves(pseudolegal_moves, game)
}

/// Given some game state, return all the (pseudo)legal tarmaps for that game. A
/// tarmap maps one position to multiple targets, which is a more efficient
/// packing than having individual Moves from one pos to another pos!
pub fn legal_tarmaps(game: Game) -> TarmapCollection {
  let origins = game.player_positions(game)

  set.fold(origins, dict.new(), fn(accum, origin) {
    let assert Ok(targets) = targets.from_pos(game, origin)

    dict.insert(accum, origin, targets)
  })
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
    targets
    |> list.map(fn(target) {
      let change = change.Change(origin, target.destination)
      move.Move(change, target.kind)
    })
    |> check.filter_pseudolegal_moves(game)
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
