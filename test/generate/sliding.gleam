import birdie
import chess/game
import legal/move

import chess/board
import chess/position
import chess/square

import legal/change.{Change}
import legal/generate

pub fn queen_goes_up_test() {
  let game = game.initial()

  // Get rid of the pawn on d2, so we can get the legal moves of the queen behind
  let assert Ok(pawn_pos) = position.new("d2")
  let game =
    game.setup_board(game, fn(board) {
      board |> board.set_pos(pawn_pos, square.None)
    })

  let assert Ok(queen_pos) = position.new("d1")
  let assert Ok(legal_moves) = generate.choices_from(game, queen_pos)

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
  let game =
    game.setup_board(game, fn(board) {
      board |> board.set_pos(pawn_pos, square.None)
    })

  let assert Ok(bishop_pos) = position.new("f1")
  let assert Ok(legal_moves) = generate.choices_from(game, bishop_pos)

  generate.display(legal_moves, game)
  |> birdie.snap(
    "Bishop on f1 with no pawn on e2 can go on that whole diagonal: a6, b5, c4, d3, and e2!",
  )
}

pub fn rook_test() {
  let game = game.initial()

  // Obviously very illegal, but lets us test what the rook can see
  let assert Ok(old_pos) = position.new("h1")
  let assert Ok(new_pos) = position.new("d4")

  let h1_rook_to_d4 = Change(old_pos, new_pos) |> move.Basic
  let game = move.apply(game, h1_rook_to_d4)

  let assert Ok(legal_moves) = generate.choices_from(game, new_pos)

  generate.display(legal_moves, game)
  |> birdie.snap("Rook on d4 can see all of rank 4, and d{3, 7}!")
}
