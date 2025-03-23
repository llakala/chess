import birdie
import chess/board
import chess/position
import gleam/string

pub fn move_passing_test() {
  let board = board.initial()

  let assert Ok(from) = position.from_algebraic("e2")
  let assert Ok(to) = position.from_algebraic("e4")

  let assert Ok(board) = board |> board.move(from, to)

  let assert Ok(str) =
    board
    |> board.to_string

  str |> birdie.snap(title: "Pawn from e2 to e4!")
}

pub fn move_failing_test() {
  let board = board.initial()

  // Purposefully empty starting square
  let assert Ok(from) = position.from_algebraic("e3")

  let assert Ok(to) = position.from_algebraic("e4")

  let move_error = board |> board.move(from, to)

  move_error |> string.inspect |> birdie.snap(title: "No piece to be moved!")
}
