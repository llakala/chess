import chess/game.{type Game}
import gleam/int
import iv
import legal/generate
import position/move.{type Move}

/// Always choose the first move in the list of moves
pub fn first(game: Game) -> Move {
  let moves = generate.legal_moves(game)

  case moves {
    [] -> panic as "No moves to select!"
    [first, ..] -> first
  }
}

pub fn random(game: Game) -> Move {
  let moves = generate.legal_moves(game) |> iv.from_list
  let index = int.random(moves |> iv.length)

  case iv.get(moves, index) {
    Ok(move) -> move
    Error(Nil) -> panic as "No moves to select!"
  }
}
