import chess/coord
import gleeunit/should

pub fn from_algebraic_test() {
  "h8" |> coord.from_algebraic |> should.equal(coord.from_pair(7, 7) |> Ok)
  "a1" |> coord.from_algebraic |> should.equal(coord.from_pair(0, 0) |> Ok)
}
