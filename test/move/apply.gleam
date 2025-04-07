import birdie
import chess/board
import chess/game
import chess/position
import legal/move.{Move}

pub fn move_forward_test() {
  let game = game.initial()

  let assert Ok(from) = position.new("e2")
  let assert Ok(to) = position.new("e4")
  let move = Move(from, to)

  let game = move.apply(game, move)

  game.board |> board.to_string |> birdie.snap(title: "Pawn from e2 to e4!")
}

pub fn move_capture_test() {
  let game = game.initial()

  let assert Ok(from) = position.new("e2")
  let assert Ok(to) = position.new("e7")
  let move = Move(from, to)

  let game = move.apply(game, move)

  game.board
  |> board.to_string
  |> birdie.snap(title: "Pawn from e2 to e7!")
}
