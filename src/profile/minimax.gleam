import bot/mini
import chess/game
import glychee/benchmark
import glychee/configuration

pub fn main() {
  configuration.initialize()
  configuration.set_pair(configuration.Time, 20)

  benchmark.run(
    [
      benchmark.Function(label: "minimax", callable: fn(game) {
        fn() { mini.max(game, 2) }
      }),
    ],
    [
      benchmark.Data(label: "Initial game", data: game.initial()),
      benchmark.Data(label: "Random game", data: {
        let assert Ok(game) =
          game.new(
            "1r2kbn1/pQ1bp1p1/n2p4/q1p2p1r/2P4p/P1N1PN2/1P1P1PPP/R1B1KB1R w KQ - 1 12",
          )
        game
      }),
    ],
  )
}
