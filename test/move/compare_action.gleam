import birdie
import chess/position
import gleam/string
import legal/action
import legal/change

pub fn basic_then_capture_test() {
  let assert Ok(a1) = position.new("a1")
  let assert Ok(a2) = position.new("a2")
  let basic = change.Change(a1, a2) |> action.Basic
  let capture = change.Change(a2, a1) |> action.Capture

  action.compare(basic, capture)
  |> string.inspect
  |> birdie.snap(
    "When comparing a Basic action and a Capture action, expected Lt!",
  )
}

pub fn neither_basic_test() {
  let assert Ok(a1) = position.new("a1")
  let assert Ok(a2) = position.new("a2")
  let basic = change.Change(a1, a2) |> action.Passant
  let capture = change.Change(a1, a2) |> action.Capture

  action.compare(basic, capture)
  |> string.inspect
  |> birdie.snap(
    "When comparing two actions that are the exact same, other than one being Passant and one being Capture, expected Eq!",
  )
}

pub fn fallback_test() {
  let assert Ok(a1) = position.new("a1")
  let assert Ok(a2) = position.new("a2")
  let assert Ok(a3) = position.new("a3")
  let basic = change.Change(a1, a2) |> action.Basic
  let capture = change.Change(a1, a3) |> action.Basic

  action.compare(basic, capture)
  |> string.inspect
  |> birdie.snap(
    "When comparing two actions with the same type, but a clear Lt relationship with their internal changes, expected Lt!",
  )
}
