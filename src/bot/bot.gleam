import chess/game
import gleam/int
import iv
import legal/move.{type Move}
import legal/moves.{type MovesFromPosition}

/// Always choose the first position, and the first move from that position
pub fn first(
  _game: game.Game,
  moves: List(MovesFromPosition),
) -> moves.MovesFromPosition {
  case moves {
    [] -> panic as "No moves to select!"
    [position_moves, ..] -> position_moves
  }
}

pub fn random(_game: game.Game, moves: List(Move)) -> Move {
  let moves = moves |> iv.from_list
  let index = int.random(moves |> iv.length)
  case iv.get(moves, index) {
    Ok(move) -> move
    Error(Nil) -> panic as "No moves to select!"
  }
}
