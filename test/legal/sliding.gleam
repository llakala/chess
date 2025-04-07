import birdie
import chess/game

import chess/board
import chess/position
import chess/square

import gleam/list
import gleam/string

import legal/generate
import legal/move

pub fn queen_goes_up_test() {
  let game = game.initial()

  // Get rid of the pawn on e2, so we can get the legal moves of the queen behind it
  let assert Ok(pawn_pos) = position.new("e2")
  let game =
    game.setup_board(game, fn(board) {
      board |> board.set_pos(pawn_pos, square.None)
    })

  let assert Ok(queen_pos) = position.new("e1")
  let assert Ok(legal_moves) = generate.legal_moves(game, queen_pos)

  legal_moves
  |> list.map(fn(move) { move |> move.to_string })
  |> string.inspect
  |> birdie.snap("Queen on e1 with no pawn in front of it can go to e{2-7}!")
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
  let assert Ok(legal_moves) = generate.legal_moves(game, bishop_pos)

  legal_moves
  |> list.map(fn(move) { move |> move.to_string })
  |> string.inspect
  |> birdie.snap(
    "Bishop on f1 with no pawn on e2 can go to e2, d3, c4, b5, and a7!",
  )
}

pub fn rook_test() {
  let game = game.initial()

  // Obviously very illegal, but lets us test what the rook can see
  let assert Ok(old_pos) = position.new("h1")
  let assert Ok(new_pos) = position.new("d4")

  let game =
    game.setup_board(game, fn(board) {
      let assert Ok(h1_rook_to_d4) = move.new(board, old_pos, new_pos)
      let assert Ok(board) = move.apply(board, h1_rook_to_d4)
      board
    })

  let assert Ok(legal_moves) = generate.legal_moves(game, new_pos)

  legal_moves
  |> list.map(fn(move) { move |> move.to_string })
  |> string.inspect
  |> birdie.snap("Rook on d4 can see all of rank 4, and d{3, 7}!")
}
