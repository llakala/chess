import chess/game
import glychee/benchmark
import glychee/configuration

pub fn main() {
  configuration.initialize()
  configuration.set_pair(configuration.Warmup, 2)
  configuration.set_pair(configuration.Parallel, 2)

  benchmark.run(
    [
      benchmark.Function(label: "fen", callable: fn(fen) {
        fn() { game.new(fen) }
      }),
    ],
    [
      benchmark.Data(label: "initial", data: {
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      }),
    ],
  )
}
