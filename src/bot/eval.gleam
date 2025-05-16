import chess/game.{type Game}
import gleam/int
import gleam/order
import piece/color.{type Color}
import position/move.{type Move}

const pawn_value = 1

const knight_value = 3

const bishop_value = 3

const rook_value = 5

const queen_value = 9

pub type EvaluatedMove {
  EvaluatedMove(move: move.Move, score: Int)
}

pub fn compare(first: EvaluatedMove, second: EvaluatedMove) -> order.Order {
  int.compare(first.score, second.score)
}

/// Given a move, evaluate it.
fn game(game: Game) -> Int {
  let player_color = game.color
  let enemy_color = color.invert(player_color)

  let player_eval = for_color(game, player_color)
  let enemy_eval = for_color(game, enemy_color)

  player_eval - enemy_eval
}

fn for_color(game: Game, color: Color) -> a {
  todo
}
