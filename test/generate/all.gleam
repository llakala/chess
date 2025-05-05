import birdie
import chess/board
import chess/game
import legal/generate
import piece/color

pub fn white_pawns_test() {
  // Empty board, other than rank 2 being full of white pawns
  let assert Ok(board) = board.new("8/8/8/8/8/8/PPPPPPPP/8")
  let game = game.Game(..game.initial(), color: color.White, board:)

  let moves = game |> generate.legal_moves

  generate.display(moves, game)
  |> birdie.snap(
    "Expected all the pawns on rank 2 to be able to move forward to ranks 3 and 4!",
  )
}

pub fn black_knights_test() {
  let assert Ok(board) = board.new("n7/8/8/8/8/6P1/8/7n")
  let game = game.Game(..game.initial(), color: color.Black, board:)

  let moves = game |> generate.legal_moves

  generate.display(moves, game)
  |> birdie.snap(
    "Expected both the knights to be able to move to 2 positions, with the h1 knight able to capture!",
  )
}

pub fn all_queens_test() {
  // Board entirely full of black queens
  let assert Ok(board) =
    board.new(
      "qqqqqqqq/qqqqqqqq/qqqqqqqq/qqqqqqqq/qqqqqqqq/qqqqqqqq/qqqqqqqq/qqqqqqqq",
    )
  let game = game.Game(..game.initial(), color: color.Black, board:)

  let moves = game |> generate.legal_moves

  generate.display(moves, game)
  |> birdie.snap(
    "Expected no valid moves, since the queens would all bump into each other!",
  )
}

pub fn no_white_moves_test() {
  // Empty board other than a black bishop on d2
  let assert Ok(board) = board.new("8/8/8/8/8/8/3b4/8")

  let game = game.Game(..game.initial(), color: color.White, board:)

  let moves = game |> generate.legal_moves

  generate.display(moves, game)
  |> birdie.snap(
    "Expected no legal moves, since there are no white pieces on the board!",
  )
}

pub fn leave_check_test() {
  let assert Ok(game) = game.new("3r4/8/8/8/3K4/7R/8/2k5 w - - 0 1")

  generate.legal_moves(game)
  |> generate.display(game)
  |> birdie.snap(
    "All the legal moves for a player that's currently in check should take them out of check!",
  )
}
