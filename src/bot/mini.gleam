import bot/eval.{EvaluatedMove}
import chess/game.{type Game}
import gleam/int
import gleam/list
import legal/apply
import legal/generate
import piece/color.{type Color, Black, White}
import position/move.{type Move}
import utils/utilist

/// Run minimax on some game, to some specified depth. With alpha beta pruining!
pub fn max(game game: Game, depth depth: Int) -> Move {
  let evaled_moves =
    generate.legal_moves(game)
    // Map each move to an EvaluatedMove, so we can select the move with the
    // best score. We have this separate from the underlying algorithm, since
    // that algorithm just selects scores, and totally ignores moves - so we
    // need this top-level to actually pick a move
    |> list.map(fn(move) {
      // Apply each legal move from the current game to the board
      let assert Ok(game) = apply.move(game, move)

      let score =
        game
        // See the game from the enemy's perspective
        |> game.flip
        // Get the score if the enemy makes the best move for them, and we make
        // the best move in response, so on recursively.
        |> minimax_loop(
          depth:,
          alpha: terrible_score(color.White),
          beta: terrible_score(color.Black),
        )

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

type EvalStatus {
  EvalStatus(alpha: Int, beta: Int, score: Int)
}

/// Get the evalyation score after recursing to some depth, always making the
/// best move we can.
fn minimax_loop(
  game game: Game,
  depth current_depth: Int,
  alpha alpha: Int,
  beta beta: Int,
) -> Int {
  case current_depth {
    // If we've reached a depth of 0, stop recursing, and return whatever the
    // current evaluation of the board is. This lets us decide whether the move
    // we performed was actually good.
    0 -> eval.game_state(game)

    _ -> {
      let moves = generate.legal_moves(game)
      let terrible_score = terrible_score(game.color)

      case moves {
        // If there's no legal moves, we're currently in checkmate! This is
        // bad for whatever color we're currently searching as - but great for
        // the other player.
        [] -> terrible_score

        _ -> {
          // We pass the current alpha, current beta, and the best score so far
          // between each iteration, so we can update them as we go. We start
          // out with a terrible score, so we things we find will be better.
          let starting_status = EvalStatus(score: terrible_score, alpha:, beta:)

          let eval_status =
            //  For each legal move, recurse one level deeper. We use
            //  `fold_until` to simulate a `break` statement for alpha-beta
            //  pruning
            list.fold_until(moves, starting_status, fn(eval_status, move) {
              move_logic(move, eval_status, game, current_depth)
            })

          // While looping, we need access to the alpha and beta - but once
          // we're done, we just need the score as our return value
          eval_status.score
        }
      }
    }
  }
}

fn move_logic(move, eval_status, game, current_depth) {
  let EvalStatus(score: best_score, alpha:, beta:) = eval_status

  // The new game, after applying the current move
  let assert Ok(game) = apply.move(game, move)

  let score =
    game
    // See the game from the enemy's perspective
    |> game.flip
    // Get the enemy's best response to the move we just made
    |> minimax_loop(current_depth - 1, alpha, beta)

  // White maximizes their score, Black minimizes it, since eval is
  // positive if white is doing well, and vice versa for black.
  let #(alpha, beta) = case game.color {
    White -> {
      let alpha = int.max(alpha, score)
      #(alpha, beta)
    }

    Black -> {
      let beta = int.min(beta, score)
      #(alpha, beta)
    }
  }

  let score = case game.color {
    White -> int.max(best_score, score)
    Black -> int.min(best_score, score)
  }

  let eval_status = EvalStatus(alpha, beta, score)

  // If this is true, we get to exit early!
  case beta <= alpha {
    True -> list.Stop(eval_status)
    False -> list.Continue(eval_status)
  }
}

fn terrible_score(color: Color) {
  case color {
    White -> -999_999
    Black -> 999_999
  }
}
