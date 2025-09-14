import chess/board
import chess/change.{type Change, Change}
import chess/color
import chess/game.{type Game}
import chess/move.{
  type Move, Basic, Capture, KingCastle, Passant, Promotion, PromotionCapture,
  QueenCastle,
}
import chess/piece.{type Piece}
import chess/square
import gleam/int
import gleam/order.{type Order}

/// Returns the value of some piece. Value is positive for white pieces, and
/// negative for black pieces.
pub fn piece(piece: Piece) -> Int {
  let value = piece_value(piece)

  let sign = case piece.color {
    color.White -> 1
    color.Black -> -1
  }

  sign * value
}

fn piece_value(piece: Piece) -> Int {
  case piece.kind {
    piece.Pawn -> 1
    piece.Knight -> 3
    piece.Bishop -> 3

    piece.Queen -> 9
    piece.Rook -> 5

    // I should probably eventually give this a value, to de-incentivize check so
    // we end up not even needing check logic for move application - but not
    // bothering with it for now.
    piece.King -> 0
  }
}

/// Compare two moves based on their kind, to figure out how good they might be.
pub fn compare_moves(game: Game, first: Move, second: Move) -> Order {
  let first_value = move(first, game)
  let second_value = move(second, game)

  // We compare backwards, so moves with high scores end up first
  int.compare(second_value, first_value)
}

/// Given some kind of move, return its value.
pub fn move(move: Move, game: Game) {
  case move.kind {
    Basic -> 0
    QueenCastle | KingCastle -> 5
    Passant -> 10

    // Captures include the value of the captured piece. If we had the time, we
    // would make the capture constructor hold the type of captured piece, but
    // that's not in scope right now, so this'll do.
    Capture -> 10 + captured_value(move.change, game)

    Promotion(piece) -> 20 + piece_value(piece)
    PromotionCapture(piece) -> 30 + piece_value(piece)
  }
}

fn captured_value(change: Change, game: Game) -> Int {
  let Change(to: destination, ..) = change
  let target_piece = board.get_pos(game.board, destination)
  case target_piece {
    // In case the capture somehow went to an empty square.
    square.None -> 0

    square.Some(piece:) -> piece_value(piece)
  }
}
