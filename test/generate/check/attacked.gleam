import birdie
import chess/board
import chess/game.{Game}
import legal/check
import piece/color

pub fn initial_test() {
  let game = Game(..game.initial(), color: color.Black)
  let attacked_squares = check.attacked_squares(game)
  check.display(game, attacked_squares)
  |> birdie.snap("Expected to see ranks 5 and 6 highlighted from black!")
}

pub fn enemy_queen_test() {
  // Checkmate position for white!
  let assert Ok(board) = board.new("8/8/8/8/8/b1n5/2n5/K6k")
  let game = Game(..game.initial(), board:, color: color.Black)

  let attacked_squares = check.attacked_squares(game)
  check.display(game, attacked_squares)
  |> birdie.snap(
    "Expected to see white in checkmate with no legal moves around its king!",
  )
}
