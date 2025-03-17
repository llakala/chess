import chess/position
import gleam/result
import gleeunit/should

pub fn from_algebraic_test() {
  "h8" |> position.from_algebraic |> should.equal(position.new(7, 7))
  "a1" |> position.from_algebraic |> should.equal(position.new(0, 0))
}

pub fn to_algebraic_test() {
  use pos <- result.try(position.new(0, 0))
  pos |> position.to_algebraic |> should.equal("a1")

  use pos <- result.try(position.new(7, 7))
  pos |> position.to_algebraic |> should.equal("h8")

  Ok(Nil)
}

pub fn from_pair_test() {
  position.new(0, 0) |> should.equal(position.Position(0, 0) |> Ok)
  position.new(7, 7) |> should.equal(position.Position(7, 7) |> Ok)

  position.new(8, 0)
  |> should.equal(
    "Invalid column `8` passed. Columns can only have value 0-7" |> Error,
  )

  position.new(0, 8)
  |> should.equal(
    "Invalid row `8` passed. Rows can only have value 0-7" |> Error,
  )
}
