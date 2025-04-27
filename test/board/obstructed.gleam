import birdie
import chess/game
import chess/sliding
import gleam/string
import piece/color
import position/position

/// Piece on E2 has a distance of 5 until the black pawn in front of it, because
/// of capturing.
pub fn until_enemy_test() {
  // Initial game has the color set to white, which is what we want.
  let game = game.initial()
  let assert Ok(pos) = position.new("e2")
  let dir = sliding.Up

  let dist = game.obstructed_distance(game, pos, dir)

  dist
  |> string.inspect
  |> birdie.snap("Distance up from e2 should be 5 with a capture!")
}

// Black pawn on e2 has a distance of 4, because it can't eat its friend on e7
pub fn until_friend_test() {
  // Default game, except the current color is black
  let game = game.Game(..game.initial(), color: color.Black)

  let assert Ok(pos) = position.new("e2")
  let dir = sliding.Up

  let dist = game.obstructed_distance(game, pos, dir)
  dist
  |> string.inspect
  |> birdie.snap("Distance up from e2 should be 4 without a capture!")
}

pub fn hit_wall_test() {
  // Initial game has color of White, which is what we want
  let game = game.initial()

  let assert Ok(pos) = position.new("e1")
  let dir = sliding.Down

  let dist = game.obstructed_distance(game, pos, dir)
  dist
  |> string.inspect
  |> birdie.snap(
    "Distance down from e1 should be 0 without a capture - hit a wall!",
  )
}
