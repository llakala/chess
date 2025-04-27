import chess/game.{type Game}
import gleam/int
import iv
import position/move.{type Move}

/// Always choose the first move in the list of moves
pub fn first(_game: Game, moves: List(Move)) -> Move {
  case moves {
    [] -> panic as "No moves to select!"
    [first, ..] -> first
  }
}

pub fn random(_game: Game, moves: List(Move)) -> Move {
  let moves = moves |> iv.from_list
  let index = int.random(moves |> iv.length)
  case iv.get(moves, index) {
    Ok(move) -> move
    Error(Nil) -> panic as "No moves to select!"
  }
}
