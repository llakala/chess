import birdie
import gleam/string
import gleeunit/should
import position/position

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

pub fn to_data_index_test() {
  let assert Ok(pos) = position.new("a8")

  pos
  |> position.to_data_index
  |> string.inspect
  |> birdie.snap("Expected a8 to have data index 0!")
}

pub fn from_data_index_test() {
  let assert Ok(pos) = position.new("a1")

  pos
  |> position.to_data_index
  |> string.inspect
  |> birdie.snap("Expected a1 to have player index 0!")
}
