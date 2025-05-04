import birdie
import chess/game
import gleam/string
import legal/check

pub fn initial_test() {
  let game = game.initial()
  let assert Ok(is_in_check) = check.is_in_check(game)

  is_in_check
  |> string.inspect
  |> birdie.snap(
    "Whether white is currently in check at the initial game state.",
  )
}

pub fn check_test() {
  let assert Ok(game) = game.new("3r4/8/8/8/3K4/7R/8/2k5 w - - 0 1")

  let assert Ok(is_in_check) = check.is_in_check(game)

  is_in_check
  |> string.inspect
  |> birdie.snap("Whether white is in check (it currently should be)")
}
