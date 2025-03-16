import chess/coord
import gleam/result
import gleeunit/should

pub fn from_algebraic_test() {
  "h8" |> coord.from_algebraic |> should.equal(coord.from_pair(7, 7))
  "a1" |> coord.from_algebraic |> should.equal(coord.from_pair(0, 0))
}

pub fn to_algebraic_test() {
  use pos <- result.try(coord.from_pair(0, 0))
  pos |> coord.to_algebraic |> should.equal("a1")

  use pos <- result.try(coord.from_pair(7, 7))
  pos |> coord.to_algebraic |> should.equal("h8")

  Ok(Nil)
}

pub fn from_pair_test() {
  coord.from_pair(0, 0) |> should.equal(coord.Coord(0, 0, 0) |> Ok)
  coord.from_pair(7, 7) |> should.equal(coord.Coord(7, 7, 63) |> Ok)

  coord.from_pair(8, 0)
  |> should.equal(
    "Invalid column `8` passed. Columns can only have value 0-7" |> Error,
  )
}
