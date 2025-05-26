import birdie
import chess/game
import gleam/pair
import legal/generate
import legal/tarmap
import position/position

pub fn display_test() {
  let game = game.initial()
  let tarmaps = generate.legal_tarmaps(game)
  tarmaps
  |> tarmap.display(game)
  |> birdie.snap("Tarmaps display for the initial board!")
}

pub fn partition_test() {
  let game = game.initial()
  let tarmaps = generate.legal_tarmaps(game)

  tarmaps
  |> tarmap.partition(fn(origin, _) {
    let assert Ok(e2) = position.new("e2")
    origin == e2
  })
  |> pair.map_first(tarmap.display(_, game))
  |> pair.map_second(tarmap.display(_, game))
  |> pair.first
  |> birdie.snap(
    "Expected to see only the e2 tarmaps, since we partitioned them out!",
  )
}
