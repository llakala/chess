import birdie
import chess/position
import gleam/string
import gleeunit/should

pub fn from_algebraic_test() {
  "h8" |> position.new |> should.equal(position.from_index(7, 7))
  "a1" |> position.new |> should.equal(position.from_index(0, 0))
}

pub fn to_algebraic_test() {
  let assert Ok(pos) = position.from_index(0, 0)
  pos |> position.to_algebraic |> should.equal("a1")

  let assert Ok(pos) = position.from_index(7, 7)
  pos |> position.to_algebraic |> should.equal("h8")
}

pub fn failing_new_test() {
  position.from_index(8, 0)
  |> string.inspect
  |> birdie.snap(title: "Invalid column")

  position.from_index(0, 8)
  |> string.inspect
  |> birdie.snap(title: "Invalid row")
}
