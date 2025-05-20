import bot/eval.{EvaluatedMove}
import chess/game.{type Game}
import gleam/int
import gleam/list
import gleam/order.{Eq, Gt, Lt}
import legal/apply
import legal/generate
import position/move.{type Move}

pub fn max(game: Game, depth: Int) -> Move {
  let moves = generate.legal_moves(game)

  let evaled_moves =
    list.map(moves, fn(move) {
      let assert Ok(game) = apply.move(game, move)

      let score =
        game
        // See the game from the enemy's perspective
        |> game.flip
        // Get the enemy's best response to the move we just made
        |> minimax_loop(depth)
        // The enemy's best response was scored for them - flip it to be
        // us-friendly!
        |> int.multiply(-1)

      // move |> move.to_string |> echo
      // echo score
      // echo "---"

      EvaluatedMove(move, score)
    })

  let assert Ok(best_evaled_move) =
    list.reduce(evaled_moves, fn(best_move, move) {
      case int.compare(best_move.score, move.score) {
        Gt -> best_move
        Lt -> move

        // If they're the same, choose at random
        Eq ->
          case int.random(2) {
            0 -> best_move
            _ -> move
          }
      }
    })

  best_evaled_move.move
}

/// Get the best potential score
fn minimax_loop(game: Game, current_depth: Int) -> Int {
  case current_depth {
    0 -> {
      // game |> game.to_string |> io.println_error
      eval.game_state(game)
    }

    _ -> {
      let moves = generate.legal_moves(game)

      use best_score, move <- list.fold(moves, -999_999)
      let assert Ok(game) = apply.move(game, move)

      let score =
        game
        // See the game from the enemy's perspective
        |> game.flip
        // Get the enemy's best response to the move we just made
        |> minimax_loop(current_depth - 1)
        // The enemy's best response was scored for them - flip it to be
        // us-friendly!
        |> int.multiply(-1)

      case int.compare(best_score, score) {
        Eq -> best_score
        Gt -> best_score
        Lt -> score
      }
    }
  }
}
