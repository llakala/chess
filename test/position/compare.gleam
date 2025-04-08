import birdie
import chess/position
import gleam/string

pub fn different_rank_test() {
  let assert Ok(a1) = position.new("a1")
  let assert Ok(b1) = position.new("b1")

  position.compare(a1, b1)
  |> string.inspect
  |> birdie.snap("Expected Lt when comparing a1 and b1!")
}

pub fn different_file_test() {
  let assert Ok(a1) = position.new("a1")
  let assert Ok(a2) = position.new("a2")

  position.compare(a1, a2)
  |> string.inspect
  |> birdie.snap("Expected Lt when comparing a1 and a2!")
}

pub fn rank_before_file_test() {
  let assert Ok(a8) = position.new("a8")
  let assert Ok(b1) = position.new("b1")

  position.compare(a8, b1)
  |> string.inspect
  |> birdie.snap(
    "Expected Lt when comparing a8 and b1, since we prioritize rank!",
  )
}
