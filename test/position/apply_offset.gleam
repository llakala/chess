import birdie
import chess/direction
import chess/offset
import chess/position
import gleam/string

pub fn one_forward_test() {
  let one_up = offset.in_direction(direction.Up, 1)
  let assert Ok(pos) = position.new("e2")
  let assert Ok(new_pos) = position.apply_offset(pos, one_up)
  new_pos
  |> position.to_string
  |> birdie.snap("Expected one up from e2 to be e3!")
}

pub fn down_left_test() {
  let down_left = offset.in_direction(direction.DownLeft, 3)
  let assert Ok(pos) = position.new("g5")
  let assert Ok(new_pos) = position.apply_offset(pos, down_left)
  new_pos
  |> position.to_string
  |> birdie.snap("Expected three down right from g5 to be d2!")
}

pub fn hit_wall_test() {
  let down_left = offset.in_direction(direction.UpRight, 8)
  let assert Ok(pos) = position.new("a1")
  let applied_result = position.apply_offset(pos, down_left)
  applied_result
  |> string.inspect
  |> birdie.snap("Expected applying an invalid offset to give an error!")
}
