import chess/game
import legal/move.{type Move}

/// Always choose the first move in the list of moves
pub fn first(_game: game.Game, moves: List(Move)) -> Move {
  case moves {
    [] -> panic as "No moves to select!"
    [first, ..] -> first
  }
}
