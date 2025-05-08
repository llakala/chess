import chess/game
import glychee/benchmark
import glychee/configuration
import legal/generate

pub fn main() {
  configuration.initialize()
  configuration.set_pair(configuration.Warmup, 2)
  configuration.set_pair(configuration.Parallel, 2)

  benchmark.run(
    [
      benchmark.Function(label: "legal_moves", callable: fn(game) {
        fn() { generate.legal_moves(game) }
      }),
    ],
    [benchmark.Data(label: "inital game", data: { game.initial() })],
  )
}
