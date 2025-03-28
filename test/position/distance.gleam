import birdie
import chess/position
import chess/sliding
import gleam/int

pub fn left_test() {
  let assert Ok(pos) = position.new("e2")
  let dir = sliding.Left
  let dist = position.distance_to_edge(pos, dir)
  dist |> int.to_string |> birdie.snap("Distance left from e2 should be 4!")
}

pub fn right_test() {
  let assert Ok(pos) = position.new("a8")
  let dir = sliding.Right
  let dist = position.distance_to_edge(pos, dir)
  dist |> int.to_string |> birdie.snap("Distance right from a8 should be 7!")
}

pub fn up_test() {
  let assert Ok(pos) = position.new("h2")
  let dir = sliding.Up
  let dist = position.distance_to_edge(pos, dir)
  dist |> int.to_string |> birdie.snap("Distance up from h2 should be 6!")
}

pub fn down_test() {
  let assert Ok(pos) = position.new("g3")
  let dir = sliding.Down
  let dist = position.distance_to_edge(pos, dir)
  dist |> int.to_string |> birdie.snap("Distance down from g3 should be 2!")
}

pub fn up_left_test() {
  let assert Ok(pos) = position.new("a4")
  let dir = sliding.UpLeft
  let dist = position.distance_to_edge(pos, dir)
  dist |> int.to_string |> birdie.snap("Distance upleft from a4 should be 0!")
}

pub fn up_right_test() {
  let assert Ok(pos) = position.new("e6")
  let dir = sliding.UpRight
  let dist = position.distance_to_edge(pos, dir)
  dist |> int.to_string |> birdie.snap("Distance upright from e6 should be 2!")
}

pub fn down_left_test() {
  let assert Ok(pos) = position.new("e5")
  let dir = sliding.DownLeft
  let dist = position.distance_to_edge(pos, dir)
  dist |> int.to_string |> birdie.snap("Distance downleft from e5 should be 4!")
}

pub fn down_right_test() {
  let assert Ok(pos) = position.new("e8")
  let dir = sliding.DownRight
  let dist = position.distance_to_edge(pos, dir)
  dist
  |> int.to_string
  |> birdie.snap("Distance downright from e8 should be 3!")
}
