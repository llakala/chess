import birdie
import chess/board
import chess/game
import chess/position
import legal/move
import legal/change.{Change}

pub fn move_forward_test() {
  let game = game.initial()

  let assert Ok(from) = position.new("e2")
  let assert Ok(to) = position.new("e4")
  let move = Change(from, to) |> move.Basic

  let game = move.apply(game, move)

  game.board |> board.to_string |> birdie.snap(title: "Pawn from e2 to e4!")
}

pub fn move_capture_test() {
  let game = game.initial()

  let assert Ok(from) = position.new("e2")
  let assert Ok(to) = position.new("e7")
  let move = Change(from, to) |> move.Capture

  let game = move.apply(game, move)

  game.board
  |> board.to_string
  |> birdie.snap(title: "Pawn from e2 to e7!")
}
