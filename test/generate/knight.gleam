import birdie
import chess/board
import chess/color.{White}
import chess/game
import chess/piece.{Piece, Queen}
import chess/position
import chess/square
import legal/generate

pub fn all_options_test() {
  // Empty board, other than a knight on d4
  let assert Ok(my_board) = board.new("8/8/8/8/3N4/8/8/8")
  let assert Ok(pos) = position.new("d4")

  // Create a Game instance using my custom board
  let game =
    game.initial()
    |> game.setup_board(fn(_) { my_board })

  let assert Ok(moves) = generate.moves_from(game, pos)

  generate.display(moves, game)
  |> birdie.snap(
    "Expected all the valid knight moves on an empty board - b3, b5, c2, c6, e2, e6, f3, and f5!",
  )
}

pub fn initial_test() {
  let game = game.initial()
  // Bottom left knight
  let assert Ok(pos) = position.new("b1")

  let assert Ok(moves) = generate.moves_from(game, pos)

  generate.display(moves, game)
  |> birdie.snap(
    "Expected b1 knight on initial board to be able to move to a3 and c3!",
  )
}

pub fn capturing_test() {
  // Initial board, but with a white queen on d7, capturable by the b8 knight
  let game =
    game.initial()
    |> game.setup_board(fn(board) {
      let white_queen_square = Piece(Queen, White) |> square.Some
      let assert Ok(pos) = position.new("d7")
      board |> board.set_pos(pos, white_queen_square)
    })

  let assert Ok(pos) = position.new("b8")

  let assert Ok(moves) = generate.moves_from(game, pos)

  generate.display(moves, game)
  |> birdie.snap(
    "Given the initial board, but with a white queen on d7, expected the b8 knight to be able to move to a6, c6, and capture on d7!",
  )
}
