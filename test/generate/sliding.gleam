import birdie
import chess/apply
import chess/game
import piece/color
import position/move

import chess/board
import piece/square
import position/position

import chess/generate
import position/change.{Change}

pub fn queen_goes_up_test() {
  let game = game.initial()

  // Get rid of the pawn on d2, so we can get the legal moves of the queen behind
  let assert Ok(pawn_pos) = position.new("d2")
  let my_board = game.board |> board.set_pos(pawn_pos, square.None)
  let game = game.Game(..game, board: my_board)

  let assert Ok(queen_pos) = position.new("d1")
  let assert Ok(legal_moves) = generate.moves_from(game, queen_pos)

  generate.display(legal_moves, game)
  |> birdie.snap(
    "Queen on d1 with no pawn in front of it can move to d{2-6}, and capture to d7!",
  )
}

pub fn bishop_test() {
  let game = game.initial()

  // Get rid of the pawn on e2, so we can get the legal moves of the bishop it's
  // blocking
  let assert Ok(pawn_pos) = position.new("e2")
  let my_board = game.board |> board.set_pos(pawn_pos, square.None)
  let game = game.Game(..game, board: my_board)

  let assert Ok(bishop_pos) = position.new("f1")
  let assert Ok(legal_moves) = generate.moves_from(game, bishop_pos)

  generate.display(legal_moves, game)
  |> birdie.snap(
    "Bishop on f1 with no pawn on e2 can go on that whole diagonal: a6, b5, c4, d3, and e2!",
  )
}

pub fn rook_test() {
  let game = game.initial()

  // Obviously very illegal, but lets us test what the rook can see from d4
  let assert Ok(old_pos) = position.new("h1")
  let assert Ok(new_pos) = position.new("d4")
  let h1_rook_to_d4 = Change(old_pos, new_pos) |> move.Basic
  let game = apply.move(game, h1_rook_to_d4)

  // Applying a move flips the color, so unflip it!
  let game = game.Game(..game, color: color.White)

  let assert Ok(legal_moves) = generate.moves_from(game, new_pos)

  generate.display(legal_moves, game)
  |> birdie.snap("Rook on d4 can see all of rank 4, and d{3, 7}!")
}
