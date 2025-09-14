import birdie
import chess/position

pub fn top_left_test() {
  let assert Ok(pos) = position.from_index(0)
  pos |> position.to_string |> birdie.snap("Expected top left to be a8!")
}

pub fn bottom_right_test() {
  let assert Ok(pos) = position.from_index(63)
  pos |> position.to_string |> birdie.snap("Expected bottom right to be h1!")
}
