import bot/eval.{EvaluatedMove}
import chess/game.{type Game}
import gleam/int
import gleam/list
import legal/apply
import legal/generate
import piece/color.{Black, White}
import position/move.{type Move}
import utils/utilist

/// Run minimax on some game, to some specified depth. Currently has no
/// alpha-beta pruning, so beware of high depths!
pub fn max(game game: Game, depth depth: Int) -> Move {
  let moves = generate.legal_moves(game)

  let evaled_moves =
    // Map each move to an EvaluatedMove, so we can select the move with the
    // best score. We have this separate from the underlying algorithm, since
    // that algorithm just selects scores, and totally ignores moves - so we
    // need this top-level to actually pick a move
    list.map(moves, fn(move) {
      // Apply each legal move from the current game to the board
      let assert Ok(game) = apply.move(game, move)

      let score =
        game
        // See the game from the enemy's perspective
        |> game.flip
        // Get the score if the enemy makes the best move for them, and we make
        // the best move in response, so on recursively.
        |> minimax_loop(depth)

      // Uncomment if you need to test. This runs only on the top-level moves,
      // not all the recursive levels!
      // let str =
      //   "Move: "
      //   <> move.to_string(move)
      //   <> "\nScore: "
      //   <> int.to_string(score)
      //   <> "\n"
      //
      // io.println_error(str)

      EvaluatedMove(move, score)
    })

  // White maximizes their score, black minimizes it. We shuffle to add some
  // randomness - feel free to remove it if you're testing!
  let best_move_result = case game.color {
    White -> list.max(evaled_moves |> list.shuffle, eval.compare)
    Black -> utilist.min(evaled_moves |> list.shuffle, eval.compare)
  }

  // `list.reduce` gives an error if the move was empty. I wish I had a good
  // `NonEmptyList` type to get around this!
  case best_move_result {
    // Probably not good to do - but panicking has been helpful to find bugs in
    // my code.
    Error(_) -> panic as "No legal moves!"

    Ok(best_move) -> {
      // Good for testing and seeing what move is actually chosen each turn. I
      // wish there was a way to see "why" it was chosen - aka the game state that
      // was so good that we had to choose this move. But I don't think I have
      // the time or mental space to implement that, so this'll do for now.
      // let str =
      //   "CHOSEN MOVE: "
      //   <> move.to_string(best_move.move)
      //   <> "\nBEST SCORE: "
      //   <> int.to_string(best_move.score)
      //   <> "\n"
      //
      // io.println_error(str)

      best_move.move
    }
  }
}

/// Get the evalyation score after recursing to some depth, always making the
/// best move we can.
fn minimax_loop(game game: Game, depth current_depth: Int) -> Int {
  case current_depth {
    // If we've reached 0 depth, stop recursing, and return whatever the current
    // evaluation of the board is. This lets us decide whether the move we
    // performed was actually good.
    0 -> eval.game_state(game)

    _ -> {
      let moves = generate.legal_moves(game)

      let terrible_score = case game.color {
        White -> -999_999
        Black -> 999_999
      }

      case moves {
        // If there's no legal moves, we're currently in checkmate! This is
        // bad for whatever color we're currently searching as - but great for
        // the other player.
        [] -> terrible_score

        _ ->
          // For each legal move, recurse one level deeper. Start out with a
          // terrible score, so we can assume things we find will be better. We
          // can't use `list.reduce`, since that forces you to keep the initial
          // type, and we want to go from moves to scores.
          list.fold(moves, terrible_score, fn(best_score, move) {
            // The new game, after applying the current move
            let assert Ok(game) = apply.move(game, move)

            let score =
              game
              // See the game from the enemy's perspective, so that in the next
              // level, the enemy can choose their response
              |> game.flip
              // Get the enemy's best response to the move we just made
              |> minimax_loop(current_depth - 1)

            // White maximizes their score, Black minimizes it, since eval is
            // positive if white is doing well, and vice versa for black.
            let func = case game.color {
              White -> int.max
              Black -> int.min
            }

            // Only the best score will keep going onward and propagate to the
            // top.
            func(best_score, score)
          })
      }
    }
  }
}
