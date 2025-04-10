import birdie
import chess/position
import gleam/list
import gleam/string
import legal/change

pub fn different_to_test() {
  let assert Ok(a1) = position.new("a1")
  let assert Ok(a2) = position.new("a2")
  let assert Ok(a3) = position.new("a3")
  let change1 = change.Change(a1, a2)
  let change2 = change.Change(a1, a3)
  change.compare(change1, change2)
  |> string.inspect
  |> birdie.snap(
    "When comparing changes \"a1 -> a2\" and \"a1 -> a3\", expected order Lt!",
  )
}

pub fn different_from_test() {
  let assert Ok(a1) = position.new("a1")
  let assert Ok(a2) = position.new("a2")
  let assert Ok(a3) = position.new("a3")
  let change1 = change.Change(a2, a1)
  let change2 = change.Change(a3, a1)
  change.compare(change1, change2)
  |> string.inspect
  |> birdie.snap(
    "When comparing changes \"a2 -> a1\" and \"a3 -> a1\", expected order Lt!",
  )
}

pub fn lots_of_changes_test() {
  let assert Ok(a1) = position.new("a1")
  let assert Ok(a2) = position.new("a2")
  let assert Ok(a3) = position.new("a3")
  let change1 = change.Change(a1, a2)
  let change2 = change.Change(a1, a3)
  let change3 = change.Change(a2, a3)

  [change1, change2, change3]
  |> list.sort(change.compare)
  |> list.map(change.to_string)
  |> string.inspect
  |> birdie.snap(
    "Expected `a1 -> a2`, then `a1 -> a3`, and finally `a2 -> a3`.",
  )
}
