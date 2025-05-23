import birdie
import chess/board
import chess/game
import piece/color.{Black, White}

pub fn initial_white_test() {
  let game = game.Game(..game.initial(), color: White)

  // All the positions of the white pieces
  let positions = game |> game.player_positions

  positions
  |> board.highlight(game.board)
  |> birdie.snap("Expected all the white initial positions!")
}

pub fn initial_black_test() {
  let game = game.Game(..game.initial(), color: Black)

  // All the positions of the black pieces
  let positions = game |> game.player_positions

  positions
  |> board.highlight(game.board)
  |> birdie.snap("Expected all the black initial positions!")
}

pub fn weird_test() {
  // Full board of white bishops, other than a black knight on f7
  // I said it would be weird!
  let assert Ok(my_board) =
    board.new(
      "BBBBBBBB/BBBBBBBB/BBBBBBnB/BBBBBBBB/BBBBBBBB/BBBBBBBB/BBBBBBBB/BBBBBBBB",
    )

  let game = game.Game(..game.initial(), board: my_board, color: Black)

  let positions = game |> game.player_positions

  positions
  |> board.highlight(game.board)
  |> birdie.snap(
    "Expected the singular black knight's position to be selected, ignoring all the white bishops.",
  )
}
