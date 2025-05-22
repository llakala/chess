import birdie
import chess/game
import gleam/list
import gleam/string
import gleeunit/should
import legal/check
import legal/generate
import position/change
import position/move

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

pub fn no_bad_knight_test() {
  // Random game I found when playing the bot against itself
  let assert Ok(game) = game.new("8/3Q1nk1/1K3p2/7R/8/2P5/7P/8 b - - 10 73")
  let legal_moves = generate.legal_moves(game)

  // An illegal move it tried to perform, that put us in check
  let assert Ok(illegal_change) = change.new("f7", "h6")
  let illegal_move = illegal_change |> move.Move(move.Basic)

  legal_moves
  |> list.contains(illegal_move)
  |> should.be_false
}
