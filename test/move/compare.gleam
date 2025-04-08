import birdie
import chess/position
import gleam/string
import legal/move

pub fn different_to_test() {
  let assert Ok(a1) = position.new("a1")
  let assert Ok(a2) = position.new("a2")
  let assert Ok(a3) = position.new("a3")
  let move1 = move.Move(a1, a2)
  let move2 = move.Move(a1, a3)
  move.compare(move1, move2)
  |> string.inspect
  |> birdie.snap(
    "When comparing moves \"a1 -> a2\" and \"a1 -> a3\", expected order Lt!",
  )
}

pub fn different_from_test() {
  let assert Ok(a1) = position.new("a1")
  let assert Ok(a2) = position.new("a2")
  let assert Ok(a3) = position.new("a3")
  let move1 = move.Move(a2, a1)
  let move2 = move.Move(a3, a1)
  move.compare(move1, move2)
  |> string.inspect
  |> birdie.snap(
    "When comparing moves \"a2 -> a1\" and \"a3 -> a1\", expected order Lt!",
  )
}

pub fn lots_of_moves_test() {
  let assert Ok(a1) = position.new("a1")
  let assert Ok(a2) = position.new("a2")
  let assert Ok(a3) = position.new("a3")
  let move1 = move.Move(a2, a1)
  let move2 = move.Move(a3, a1)
  move.compare(move1, move2)
  |> string.inspect
  |> birdie.snap(
    "When comparing moves \"a2 -> a1\" and \"a3 -> a1\", expected order Lt!",
  )
}
