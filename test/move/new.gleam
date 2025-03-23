import birdie
import chess/board
import chess/move
import chess/position
import gleam/string
import gleeunit/should

pub fn new_passing_test() {
  let board: board.Board = board.initial()

  let assert Ok(from) = position.from_algebraic("e2")
  let assert Ok(to) = position.from_algebraic("e4")

  move.new(board, from, to) |> should.be_ok
}

pub fn new_failing_test() {
  let board = board.initial()

  // Purposefully empty starting square
  let assert Ok(from) = position.from_algebraic("e3")
  let assert Ok(to) = position.from_algebraic("e4")
  let new_error = move.new(board, from, to)

  new_error |> string.inspect |> birdie.snap(title: "No piece to be moved!")
}
