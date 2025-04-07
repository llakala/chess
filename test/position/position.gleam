import birdie
import chess/position
import gleam/string
import gleeunit/should

pub fn new_test() {
  "h8" |> position.new |> should.equal(position.from_indices(7, 7))
  "a1" |> position.new |> should.equal(position.from_indices(0, 0))
}

pub fn to_string_test() {
  let assert Ok(pos) = position.from_indices(0, 0)
  pos |> position.to_string |> should.equal("a1")

  let assert Ok(pos) = position.from_indices(7, 7)
  pos |> position.to_string |> should.equal("h8")
}

pub fn from_index_test() {
  position.from_indices(8, 0)
  |> string.inspect
  |> birdie.snap(title: "Invalid column")

  position.from_indices(0, 8)
  |> string.inspect
  |> birdie.snap(title: "Invalid row")
}
