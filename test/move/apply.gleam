import birdie
import chess/board
import chess/color
import chess/game
import chess/piece
import chess/position
import legal/change.{Change}
import legal/move

pub fn move_forward_test() {
  let game = game.initial()

  let assert Ok(from) = position.new("e2")
  let assert Ok(to) = position.new("e4")
  let move = Change(from, to) |> move.Basic

  let game = move.apply(game, move)

  game.board |> board.to_string |> birdie.snap(title: "Pawn from e2 to e4!")
}

pub fn move_capture_test() {
  let game = game.initial()

  let assert Ok(from) = position.new("e2")
  let assert Ok(to) = position.new("e7")
  let move = Change(from, to) |> move.Capture

  let game = move.apply(game, move)

  game.board
  |> board.to_string
  |> birdie.snap(title: "Pawn from e2 to e7!")
}

pub fn en_passant_test() {
  // Initial board with white pawn on e5, and black pawn on d5, which just double
  // moved over d6 (and set the en passant signal). White pawn can en passant!
  let assert Ok(game) =
    game.new("rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 1")

  let assert Ok(from) = position.new("e5")
  let assert Ok(to) = position.new("d6")
  let move = Change(from, to) |> move.Passant

  let game = move.apply(game, move)

  game.board
  |> board.to_string
  |> birdie.snap(
    "After performing an en passant, white pawn is on d6, after capturing the d5 pawn!",
  )
}

pub fn promotion_test() {
  // Empty board other than a pawn on a2, about to promote.
  let assert Ok(game) = game.new("8/8/8/8/8/8/p7/8 w KQkq - 0 1")
  let assert Ok(from) = position.new("a2")
  let assert Ok(to) = position.new("a1")
  let move = Change(from, to) |> move.Promotion(piece.Queen(color.Black))

  let game = move.apply(game, move)

  game.board
  |> board.to_string
  |> birdie.snap(
    "After an a2 black pawn promoted to a queen, found only a black queen on a1!",
  )
}
