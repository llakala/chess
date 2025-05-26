import birdie
import chess/game
import legal/generate
import legal/tarmap

pub fn display_test() {
  let game = game.initial()
  let tarmaps = generate.legal_tarmaps(game)
  tarmaps
  |> tarmap.display(game)
  |> birdie.snap("Tarmaps display for the initial board!")
}
