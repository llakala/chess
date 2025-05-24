import bot/mini
import chess/game
import glychee/benchmark
import glychee/configuration

pub fn main() {
  configuration.initialize()
  configuration.set_pair(configuration.Time, 20)

  benchmark.run(
    [
      benchmark.Function(label: "minimax", callable: fn(depth) {
        let game = game.initial()
        fn() { mini.max(game, depth) }
      }),
    ],
    [benchmark.Data(label: "Depth of 3", data: 3)],
  )
}
