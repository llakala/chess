import birdie
import chess/algebraic
import chess/board
import chess/game
import position/change
import position/move

pub fn pawn_forward_test() {
  let game = game.initial()

  let assert Ok(change) = change.new("e2", "e4")
  let move = change |> move.Basic
  let assert Ok(output) = algebraic.notation(move, game)

  output
  |> birdie.snap(
    "Expected move `e2 -> e4` to be represented algebraically as `e4`",
  )
}

pub fn pawn_capture_test() {
  // Game with a custom board containing a pawn on e3, able to capture the queen on
  // d4.
  let assert Ok(board) = board.new("8/8/8/8/3q4/4P3/8/8")
  let game = game.initial()
  let game = game.Game(..game, board:)

  let assert Ok(change) = change.new("e3", "d4")
  let move = change |> move.Capture
  let assert Ok(output) = algebraic.notation(move, game)

  output
  |> birdie.snap(
    "Expected a pawn capture to be represented algebraically as exd4!",
  )
}

pub fn normal_piece_test() {
  // Empty board other than a queen on a8
  let assert Ok(board) = board.new("q7/8/8/8/8/8/8/8")
  let game = game.initial()
  let game = game.Game(..game, board:)

  let assert Ok(change) = change.new("a8", "f3")
  let move = change |> move.Basic
  let assert Ok(output) = algebraic.notation(move, game)

  output
  |> birdie.snap("Expected queen from a8 -> f3 to be represented as `Qa8f3`!")
}
