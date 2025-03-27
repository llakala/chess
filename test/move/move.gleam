import birdie
import chess/board
import chess/move
import chess/position

pub fn move_forward_test() {
  let board = board.initial()

  let assert Ok(from) = position.new("e2")
  let assert Ok(to) = position.new("e4")
  let assert Ok(move) = move.new(board, from, to)

  let assert Ok(board) = board |> move.move(move)

  let assert Ok(str) =
    board
    |> board.to_string

  str |> birdie.snap(title: "Pawn from e2 to e4!")
}

pub fn move_capture_test() {
  let board = board.initial()

  let assert Ok(from) = position.new("e2")
  let assert Ok(to) = position.new("e7")
  let assert Ok(move) = move.new(board, from, to)

  let assert Ok(board) = board |> move.move(move)

  let assert Ok(str) =
    board
    |> board.to_string

  str |> birdie.snap(title: "Pawn from e2 to e7!")
}
