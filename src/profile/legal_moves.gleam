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
    [
      benchmark.Data(label: "inital game", data: { game.initial() }),
      benchmark.Data(label: "game currently in check", data: {
        let assert Ok(game) = game.new("3r4/8/8/8/3K4/7R/8/2k5 w - - 0 1")
        game
      }),
    ],
  )
}
