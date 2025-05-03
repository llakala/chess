import birdie
import gleam/string
import position/change
import position/move.{Move}

pub fn basic_then_capture_test() {
  let assert Ok(a1_to_a2) = change.new("a1", "a2")
  let basic_move = Move(a1_to_a2, move.Basic)
  let capture_move = Move(a1_to_a2, move.Capture)

  move.compare(basic_move, capture_move)
  |> string.inspect
  |> birdie.snap(
    "When comparing a Basic move and a Capture move with the same internal move, expected Eq!",
  )
}

pub fn neither_basic_test() {
  let assert Ok(change) = change.new("a1", "a2")
  let basic = Move(change, move.Passant)
  let capture = Move(change, move.Capture)

  move.compare(basic, capture)
  |> string.inspect
  |> birdie.snap(
    "When comparing two moves that are the exact same, other than one being Passant and one being Capture, expected Eq!",
  )
}

pub fn fallback_test() {
  let assert Ok(to_a2) = change.new("a1", "a2")
  let assert Ok(to_a3) = change.new("a1", "a3")
  let basic1 = Move(to_a2, move.Basic)
  let basic2 = Move(to_a3, move.Basic)

  move.compare(basic1, basic2)
  |> string.inspect
  |> birdie.snap(
    "When comparing two moves with the same type, but a clear Lt relationship with their internal changes, expected Lt!",
  )
}
