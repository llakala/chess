import birdie
import chess/board
import chess/color
import chess/position
import chess/sliding
import gleam/string

/// Piece on E2 has a distance of 5 until the black pawn in front of it, because
/// of capturing.
pub fn until_enemy_test() {
  let board = board.initial()
  let assert Ok(pos) = position.new("e2")
  let dir = sliding.Up
  let color = color.White

  let dist = board.obstructed_distance(board, pos, dir, color)
  dist
  |> string.inspect
  |> birdie.snap("Distance up from e2 should be 5 with a capture!")
}

// Black pawn on e2 has a distance of 4, because it can't eat its friend on e7
pub fn until_friend_test() {
  let board = board.initial()
  let assert Ok(pos) = position.new("e2")
  let dir = sliding.Up
  let color = color.Black

  let dist = board.obstructed_distance(board, pos, dir, color)
  dist
  |> string.inspect
  |> birdie.snap("Distance up from e2 should be 4 without a capture!")
}

pub fn hit_wall_test() {
  let board = board.initial()
  let assert Ok(pos) = position.new("e1")
  let dir = sliding.Down
  let color = color.White

  let dist = board.obstructed_distance(board, pos, dir, color)
  dist
  |> string.inspect
  |> birdie.snap(
    "Distance down from e1 should be 0 without a capture - hit a wall!",
  )
}
