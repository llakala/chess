import bot/eval.{EvaluatedMove}
import bot/score
import chess/game.{type Game}
import gleam/bool
import gleam/int
import gleam/list.{type ContinueOrStop}
import legal/apply
import legal/generate
import piece/color.{type Color, Black, White}
import position/move.{type Move}
import utils/utilist

/// Run minimax on some game, to some specified depth. With alpha beta pruining!
pub fn max(game game: Game, depth depth: Int) -> Move {
  let evaled_moves =
    generate.legal_moves(game)
    |> list.sort(score.compare_moves)
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
    White -> evaled_moves |> list.shuffle |> list.max(eval.compare)
    Black -> evaled_moves |> list.shuffle |> utilist.min(eval.compare)
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
  // If we've reached a depth of 0, stop recursing, and return whatever the
  // current evaluation of the board is. This lets us decide whether the move
  // we performed was actually good.
  use <- bool.guard(current_depth == 0, eval.game_state(game))

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
      let status = EvalStatus(score: terrible_score, alpha:, beta:)

      // While looping, we need access to the alpha and beta - but once
      // we're done, we just need the score as our return value
      let EvalStatus(score: best_score, ..) =
        get_best_score(game, moves, status, current_depth)

      best_score
    }
  }
}

fn get_best_score(
  game: Game,
  moves: List(Move),
  status: EvalStatus,
  depth: Int,
) -> EvalStatus {
  //  For each legal move, recurse one level deeper. We use
  //  `fold_until` to simulate a `break` statement for alpha-beta
  //  pruning
  list.fold_until(moves, status, fn(status, move) {
    // The new game, after applying the current move
    let assert Ok(game) = apply.move(game, move)

    let score =
      game
      // See the game from the enemy's perspective
      |> game.flip
      // Get the enemy's best response to the move we just made
      |> minimax_loop(depth - 1, status.alpha, status.beta)

    case game.color {
      White -> white_logic(status, score)
      Black -> black_logic(status, score)
    }
  })
}

fn white_logic(
  status: EvalStatus,
  current_score: Int,
) -> ContinueOrStop(EvalStatus) {
  let EvalStatus(score: best_score, alpha:, beta:) = status

  let best_score = int.max(best_score, current_score)

  let alpha = int.max(alpha, best_score)

  // Pass the best score onwards
  let eval_status = EvalStatus(alpha:, beta:, score: best_score)

  // If this is true, we get to exit early!
  case beta <= alpha {
    True -> list.Stop(eval_status)
    False -> list.Continue(eval_status)
  }
}

fn black_logic(
  status: EvalStatus,
  current_score: Int,
) -> ContinueOrStop(EvalStatus) {
  let EvalStatus(score: best_score, alpha:, beta:) = status

  let best_score = int.min(best_score, current_score)

  let beta = int.min(beta, best_score)

  // Pass the best score onwards
  let eval_status = EvalStatus(alpha:, beta:, score: best_score)

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
