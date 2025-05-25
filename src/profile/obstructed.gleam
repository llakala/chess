import chess/game
import gleam/set
import glychee/benchmark
import glychee/configuration
import position/direction

pub fn main() {
  configuration.initialize()
  configuration.set_pair(configuration.Warmup, 2)
  configuration.set_pair(configuration.Parallel, 2)

  benchmark.run(
    [
      benchmark.Function(label: "obstructed_distance", callable: fn(game) {
        fn() {
          let positions = game.player_positions(game)

          set.map(positions, fn(pos) {
            game.obstructed_distance(game, pos, direction.Up)
          })
        }
      }),
    ],
    [benchmark.Data(label: "initial", data: { game.initial() })],
  )
}
