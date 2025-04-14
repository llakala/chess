import birdie
import gleam/string
import legal/change
import legal/move

pub fn basic_then_capture_test() {
  let assert Ok(a1_to_a2) = change.new("a1", "a2")
  let assert Ok(a2_to_a1) = change.new("a2", "a1")
  let basic_move = a1_to_a2 |> move.Basic
  let capture_move = a2_to_a1 |> move.Capture

  move.compare(basic_move, capture_move)
  |> string.inspect
  |> birdie.snap("When comparing a Basic move and a Capture move, expected Lt!")
}

pub fn neither_basic_test() {
  let assert Ok(change) = change.new("a1", "a2")
  let basic = change |> move.Passant
  let capture = change |> move.Capture

  move.compare(basic, capture)
  |> string.inspect
  |> birdie.snap(
    "When comparing two moves that are the exact same, other than one being Passant and one being Capture, expected Eq!",
  )
}

pub fn fallback_test() {
  let assert Ok(a1_to_a2) = change.new("a1", "a2")
  let assert Ok(a1_to_a3) = change.new("a1", "a3")
  let basic1 = a1_to_a2 |> move.Basic
  let basic2 = a1_to_a3 |> move.Basic

  move.compare(basic1, basic2)
  |> string.inspect
  |> birdie.snap(
    "When comparing two moves with the same type, but a clear Lt relationship with their internal changes, expected Lt!",
  )
}
