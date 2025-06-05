import birdie
import chess/game
import gleam/list
import piece/color
import piece/piece.{Pawn, Piece}
import position/move.{Passant}

import chess/board
import piece/square
import position/position

import legal/generate

pub fn white_pawn_test() {
  let game = game.initial()

  // Set up a white pawn on e4
  let assert Ok(pos) = position.new("e4")
  let pawn_square = Piece(Pawn, color.White) |> square.new
  let my_board = game.board |> board.set_pos(pos, pawn_square)
  let game = game.Game(..game, board: my_board)

  let assert Ok(legal_moves) = generate.moves_from(game, pos)

  move.display(legal_moves, game)
  |> birdie.snap("White pawn on e4 can only go to e5!")
}

pub fn black_pawn_test() {
  let game = game.initial()

  // Set up a black pawn on e4
  let assert Ok(pos) = position.new("e4")
  let pawn_square = Piece(Pawn, color.Black) |> square.new
  let my_board = game.board |> board.set_pos(pos, pawn_square)
  let game = game.Game(..game, board: my_board)

  let assert Ok(legal_moves) = generate.moves_from(game, pos)

  move.display(legal_moves, game)
  |> birdie.snap("Black pawn on e4 can only go to e3!")
}

pub fn white_pawn_double_move_test() {
  let game = game.initial()

  // Set up a white pawn on e2
  let assert Ok(pos) = position.new("e2")
  let pawn_square = Piece(Pawn, color.White) |> square.new
  let my_board = game.board |> board.set_pos(pos, pawn_square)
  let game = game.Game(..game, board: my_board)

  let assert Ok(legal_moves) = generate.moves_from(game, pos)

  move.display(legal_moves, game)
  |> birdie.snap("White pawn on e2 can go to e3 and e4!")
}

pub fn black_pawn_double_move_test() {
  let game = game.initial()

  // Set up a black pawn on a7
  let assert Ok(pos) = position.new("a7")
  let pawn_square = Piece(Pawn, color.Black) |> square.new
  let my_board = game.board |> board.set_pos(pos, pawn_square)
  let game = game.Game(..game, board: my_board)

  let assert Ok(legal_moves) = generate.moves_from(game, pos)

  move.display(legal_moves, game)
  |> birdie.snap("Black pawn on a7 can go to a6 and a5!")
}

pub fn passant_test() {
  // Board encoding a white pawn and black pawn next to each other on the e rank --
  // with the black pawn just moving from d7 to d5, making en passant possible
  let assert Ok(game) =
    game.new("rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 1")

  // The position of the white pawn that can now perform en passant
  let assert Ok(pawn_pos) = position.new("e5")

  let assert Ok(legal_moves) = generate.moves_from(game, pawn_pos)

  move.display(legal_moves, game)
  |> birdie.snap("White pawn can en passant to d6, or go forward to e6!")
}

pub fn capture_test() {
  // Board encoding a black pawn on e6, which should be able to capture the pawns
  // on d7 and f7
  let assert Ok(game) =
    game.new("rnbqkbnr/pppppppp/4P3/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1")

  // The position of the white pawn that has two capture moves
  let assert Ok(pawn_pos) = position.new("e6")

  let assert Ok(legal_moves) = generate.moves_from(game, pawn_pos)

  move.display(legal_moves, game)
  |> birdie.snap("White pawn on e6 can capture on d7 or f7!")
}

pub fn promotion_test() {
  // Empty board, other than a white pawn on c7
  let assert Ok(game) = game.new("8/2P5/8/8/8/8/8/8 w KQkq - 0 1")

  let assert Ok(pos) = position.new("c7")

  let assert Ok(legal_moves) = generate.moves_from(game, pos)

  move.display(legal_moves, game)
  |> birdie.snap(
    "White pawn on c7 can promote into a rook, bishop, knight, or queen!",
  )
}

pub fn promotion_capture_test() {
  // White pawn on c7, with a white rook on c8, and a black queen on d8
  let assert Ok(game) = game.new("2Rq4/2P5/8/8/8/8/8/8 w KQkq - 0 1")
  let assert Ok(pos) = position.new("c7")

  let assert Ok(legal_moves) = generate.moves_from(game, pos)

  move.display(legal_moves, game)
  |> birdie.snap(
    "White pawn on c7 can capture on d8, promoting into a rook, bishop, knight, or queen!",
  )
}

pub fn bad_f3_test() {
  // Game I found while testing, where the wrong pawn was able to passant
  let assert Ok(game) =
    game.new(
      "1r1qkb1Q/2pn3p/1pBp2p1/p4pn1/2P1pP2/1P2P3/P2PK2P/RNB3N1 b - f3 0 19",
    )
  let moves = generate.legal_moves(game)

  // We should only find one passant move
  moves
  |> list.filter(fn(move) { move.kind == Passant })
  |> move.display(game)
  |> birdie.snap("Expected to see an en passant move from e4 -> f3!")
}
