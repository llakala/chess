import birdie
import chess/color
import chess/game
import chess/piece
import gleam/io
import legal/action

import chess/board
import chess/position
import chess/square

import gleam/list
import gleam/string

import legal/generate

pub fn white_pawn_test() {
  let game = game.initial()

  // Set up a white pawn on e4
  let assert Ok(pos) = position.new("e4")
  let pawn_square = piece.Pawn(color.White) |> square.new

  let game =
    game.setup_board(game, fn(board) { board.set_pos(board, pos, pawn_square) })

  let assert Ok(legal_actions) = generate.legal_actions(game, pos)

  legal_actions
  |> list.map(fn(action) { action |> action.to_string })
  |> string.inspect
  |> birdie.snap("White pawn on e4 can only go to e5!")
}

pub fn black_pawn_test() {
  let game = game.initial()

  // Set up a black pawn on e4
  let assert Ok(pos) = position.new("e4")
  let pawn_square = piece.Pawn(color.Black) |> square.new

  let game =
    game.setup_board(game, fn(board) { board.set_pos(board, pos, pawn_square) })

  let assert Ok(legal_actions) = generate.legal_actions(game, pos)

  legal_actions
  |> list.map(fn(action) { action |> action.to_string })
  |> string.inspect
  |> birdie.snap("Black pawn on e4 can only go to e3!")
}

pub fn white_pawn_double_move_test() {
  let game = game.initial()

  // Set up a white pawn on e2
  let assert Ok(pos) = position.new("e2")
  let pawn_square = piece.Pawn(color.White) |> square.new

  let game =
    game.setup_board(game, fn(board) { board.set_pos(board, pos, pawn_square) })

  let assert Ok(legal_actions) = generate.legal_actions(game, pos)

  legal_actions
  |> list.map(fn(action) { action |> action.to_string })
  |> string.inspect
  |> birdie.snap("White pawn on e2 can go to e3 and e4!")
}

pub fn black_pawn_double_move_test() {
  let game = game.initial()

  // Set up a black pawn on a7
  let assert Ok(pos) = position.new("a7")
  let pawn_square = piece.Pawn(color.Black) |> square.new

  let game =
    game.setup_board(game, fn(board) { board.set_pos(board, pos, pawn_square) })

  let assert Ok(legal_actions) = generate.legal_actions(game, pos)

  legal_actions
  |> list.map(fn(action) { action |> action.to_string })
  |> string.inspect
  |> birdie.snap("Black pawn on a7 can go to a6 and a5!")
}

pub fn passant_test() {
  // Board encoding a white pawn and black pawn next to each other on the e rank --
  // with the black pawn just moving from d7 to d5, making en passant possible
  let assert Ok(game) =
    game.new("rnbqkbnr/pppppppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR b KQkq d6 0 1")

  game |> game.to_string |> io.println

  // The position of the white pawn that can now perform en passant
  let assert Ok(pawn_pos) = position.new("e5")

  let assert Ok(legal_actions) = generate.legal_actions(game, pawn_pos)

  legal_actions
  |> list.map(fn(action) { action |> action.to_string })
  |> string.inspect
  |> birdie.snap("White pawn can go forward to e6, or en passant to d6!")
}

pub fn capture_test() {
  // Board encoding a black pawn on e6, which should be able to capture the pawns
  // on d7 and f7
  let assert Ok(game) =
    game.new("rnbqkbnr/pppppppp/4P3/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1")

  // The position of the white pawn that has two capture actions
  let assert Ok(pawn_pos) = position.new("e6")

  let assert Ok(legal_actions) = generate.legal_actions(game, pawn_pos)

  legal_actions
  |> list.map(fn(action) { action |> action.to_string })
  |> string.inspect
  |> birdie.snap("White pawn on e6 can capture on d7 or f7!")
}
