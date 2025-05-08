import erlang_template/chess
import glychee/benchmark
import glychee/configuration
import piece/color

pub fn main() {
  configuration.initialize()
  configuration.set_pair(configuration.Warmup, 2)
  configuration.set_pair(configuration.Parallel, 2)

  benchmark.run(
    [
      benchmark.Function(label: "player_positions", callable: fn(fen) {
        fn() { chess.move(fen, color.White, []) }
      }),
    ],
    [
      benchmark.Data(label: "pre-sorted list", data: {
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      }),
    ],
  )
}
