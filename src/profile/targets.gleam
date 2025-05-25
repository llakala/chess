import chess/game
import gleam/set
import glychee/benchmark
import glychee/configuration
import legal/targets

pub fn main() {
  configuration.initialize()
  configuration.set_pair(configuration.Warmup, 2)
  configuration.set_pair(configuration.Parallel, 2)

  benchmark.run(
    [
      benchmark.Function(label: "targets", callable: fn(game) {
        fn() {
          let positions = game.player_positions(game)

          set.map(positions, fn(pos) { targets.from_pos(game, pos) })
        }
      }),
    ],
    [benchmark.Data(label: "initial", data: { game.initial() })],
  )
}
