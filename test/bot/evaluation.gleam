import birdie
import bot/eval
import chess/game
import gleam/string

pub fn initial_test() {
  let game = game.initial()
  let score = eval.game_state(game)

  score
  |> string.inspect
  |> birdie.snap("Expected the initial game to have an evaluation score of 0!")
}

pub fn winning_test() {
  // Initial board, but the other player is missing a queen
  let assert Ok(game) =
    game.new("rnb1kbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

  eval.game_state(game)
  |> string.inspect
  |> birdie.snap(
    "Expected a game with the enemy's queen missing to have a positive evaluation score!",
  )
}

pub fn losing_test() {
  // Initial board, but we're missing a queen
  let assert Ok(game) =
    game.new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNB1KBNR w KQkq - 0 1")

  eval.game_state(game)
  |> string.inspect
  |> birdie.snap(
    "Expected a game with our queen missing to have a negative evaluation score!",
  )
}

pub fn losing_horribly_test() {
  // Other player has all their pieces, we only have our king
  let assert Ok(game) = game.new("rnb1kbnr/pppppppp/8/8/8/8/8/4K3 w kq - 0 1")

  eval.game_state(game)
  |> string.inspect
  |> birdie.snap(
    "Expected a game with all our pieces missing but our king to have a very bad evaluation score!",
  )
}
