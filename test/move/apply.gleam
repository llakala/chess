import birdie
import chess/board
import chess/game
import chess/position
import legal/action
import legal/move.{Move}

pub fn move_forward_test() {
  let game = game.initial()

  let assert Ok(from) = position.new("e2")
  let assert Ok(to) = position.new("e4")
  let action = Move(from, to) |> action.Basic

  let game = action.apply(game, action)

  game.board |> board.to_string |> birdie.snap(title: "Pawn from e2 to e4!")
}

pub fn move_capture_test() {
  let game = game.initial()

  let assert Ok(from) = position.new("e2")
  let assert Ok(to) = position.new("e7")
  let action = Move(from, to) |> action.Capture

  let game = action.apply(game, action)

  game.board
  |> board.to_string
  |> birdie.snap(title: "Pawn from e2 to e7!")
}
