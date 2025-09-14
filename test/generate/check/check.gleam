import birdie
import chess/change
import chess/game
import chess/move
import chess/position
import gleam/list
import gleam/string
import gleeunit/should
import legal/check
import legal/generate

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

pub fn sneaky_pawn_gets_knight_test() {
  // Yep, another game I found randomly
  let assert Ok(game) =
    game.new("r3kb1r/2p2ppp/1p2p3/p7/P1P3n1/4nPPP/PB1PP1K1/RN4NR w kq - 4 15")
  let legal_moves = generate.legal_moves(game)

  legal_moves
  |> list.is_empty
  |> should.be_false
}

pub fn bad_king_test() {
  let assert Ok(game) =
    game.new("r3kbQ1/pq2pp2/2pp3p/8/p4N2/2PPPn2/1P2KP1P/1NB4b w q - 0 21")

  let assert Ok(pos) = position.new("e2")
  let assert Ok(legal_moves) = generate.moves_from(game, pos)

  legal_moves
  |> move.display(game)
  |> birdie.snap(
    "Expected to not be able to move from e2 -> f3, since it's illegal!",
  )

  let assert Ok(change) = change.new("e2", "f3")
  let move = move.Move(change, move.Capture)

  check.is_move_legal(move, game)
  |> should.be_false
}

pub fn stop_misbehaving_test() {
  let assert Ok(game) = game.new("8/2p1k3/2p5/4K3/3pP3/8/4n3/8 w - - 14 39")

  let legal_moves = generate.legal_moves(game)

  legal_moves
  |> move.display(game)
  |> birdie.snap("Expected the only legal move to be e5 -> f5!")
}
