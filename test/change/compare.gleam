import birdie
import chess/change
import gleam/list
import gleam/string

pub fn different_to_test() {
  let assert Ok(change1) = change.new("a1", "a2")
  let assert Ok(change2) = change.new("a1", "a3")
  change.compare(change1, change2)
  |> string.inspect
  |> birdie.snap(
    "When comparing changes \"a1 -> a2\" and \"a1 -> a3\", expected order Lt!",
  )
}

pub fn different_from_test() {
  let assert Ok(change1) = change.new("a2", "a1")
  let assert Ok(change2) = change.new("a3", "a1")
  change.compare(change1, change2)
  |> string.inspect
  |> birdie.snap(
    "When comparing changes \"a2 -> a1\" and \"a3 -> a1\", expected order Lt!",
  )
}

pub fn lots_of_changes_test() {
  let assert Ok(change1) = change.new("a1", "a2")
  let assert Ok(change2) = change.new("a1", "a3")
  let assert Ok(change3) = change.new("a2", "a3")

  [change1, change2, change3]
  |> list.sort(change.compare)
  |> list.map(change.to_string)
  |> string.inspect
  |> birdie.snap(
    "Expected `a1 -> a2`, then `a1 -> a3`, and finally `a2 -> a3`.",
  )
}
