import birdie
import chess/board
import chess/move
import chess/position
import chess/square
import gleam/string
import gleeunit/should
import iv

pub fn queen_goes_up_test() {
  let board = board.initial()

  // Get rid of the pawn on e2, so we can get the legal moves of the queen behind it
  let assert Ok(pawn_pos) = position.new("e2")
  let assert Ok(board) = board |> board.set_pos(pawn_pos, square.None)

  // To make sure that the above logic worked, we check that there's no longer a
  // pawn at e2
  let square = board.get_pos(board, pawn_pos)
  square |> should.equal(square.None)

  let assert Ok(queen_pos) = position.new("e1")
  let assert Ok(legal_moves) = move.legal_moves(board, queen_pos)

  legal_moves
  |> iv.map(fn(move) { move |> move.to_string })
  |> iv.to_list
  |> string.inspect
  |> birdie.snap("Queen on e1 with no pawn in front of it can go to e{2-7}!")
}

pub fn bishop_test() {
  let board = board.initial()

  // Get rid of the pawn on e2, so we can get the legal moves of the bishop it's
  // blocking
  let assert Ok(pawn_pos) = position.new("e2")
  let assert Ok(board) = board |> board.set_pos(pawn_pos, square.None)

  // To make sure that the above logic worked, we check that there's no longer a
  // pawn at e2
  let square = board.get_pos(board, pawn_pos)
  square |> should.equal(square.None)

  let assert Ok(bishop_pos) = position.new("f1")
  let assert Ok(legal_moves) = move.legal_moves(board, bishop_pos)

  legal_moves
  |> iv.map(fn(move) { move |> move.to_string })
  |> iv.to_list
  |> string.inspect
  |> birdie.snap(
    "Bishop on f1 with no pawn on e2 can go to e2, d3, c4, b5, and a7!",
  )
}

pub fn rook_test() {
  let board = board.initial()

  // Obviously very illegal, but lets us test what the rook can see
  let assert Ok(old_pos) = position.new("h1")
  let assert Ok(new_pos) = position.new("d4")
  let assert Ok(h1_rook_to_d4) = move.new(board, old_pos, new_pos)
  let assert Ok(board) = move.apply(board, h1_rook_to_d4)

  let assert Ok(legal_moves) = move.legal_moves(board, new_pos)

  legal_moves
  |> iv.map(fn(move) { move |> move.to_string })
  |> iv.to_list
  |> string.inspect
  |> birdie.snap("Rook on d4 can see all of rank 4, and d{3, 7}!")
}

pub fn non_sliding_piece_test() {
  let board = board.initial()

  let assert Ok(pawn_pos) = position.new("a2")

  let legal_moves_result = move.legal_moves(board, pawn_pos)

  legal_moves_result
  |> should.be_error
}
