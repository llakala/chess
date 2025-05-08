import chess/game
import gleam/list
import glychee/benchmark
import glychee/configuration
import legal/apply
import legal/generate

pub fn main() {
  configuration.initialize()
  configuration.set_pair(configuration.Warmup, 2)
  configuration.set_pair(configuration.Parallel, 2)

  benchmark.run(
    [
      benchmark.Function(label: "targets", callable: fn(game) {
        fn() {
          let moves = generate.legal_moves(game)
          list.map(moves, fn(move) { apply.move(game, move) })
        }
      }),
    ],
    [benchmark.Data(label: "initial", data: { game.initial() })],
  )
}
