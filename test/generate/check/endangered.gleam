import birdie
import chess/board
import chess/game
import legal/check

pub fn endangered_test() {
  let assert Ok(game) = game.new("4r2k/8/8/8/4p3/3K4/8/8 w - - 0 1")

  let endangered_positions = check.endangered_positions(game)

  endangered_positions
  |> board.highlight(game.board, _)
  |> birdie.snap(
    "Expected to see the black pawn as an endangered position, even though it's an enemy!",
  )
}
