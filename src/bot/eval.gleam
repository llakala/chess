import bot/score
import chess/board
import chess/game.{type Game}
import gleam/dict
import gleam/int
import gleam/list
import gleam/order.{type Order}
import position/move.{type Move}

/// This type isn't used much within the `eval` module - instead, you should
/// apply the moves yourself, and see what score they result in.
pub type EvaluatedMove {
  EvaluatedMove(move: Move, score: Int)
}

/// Convenience compare function for sorting evaluated moves.
pub fn compare(first: EvaluatedMove, second: EvaluatedMove) -> Order {
  int.compare(first.score, second.score)
}

/// Given some game state, return an evaluation score. The score does NOT change
/// depending on the current player - it will be positive if white is winning,
/// and negative if black is winning. So, don't invert this output, and instead
/// choose to sort moves differently depending on the current player - either to
/// maximize or minimize eval!
pub fn game_state(game: Game) -> Int {
  let piece_positions = board.piece_positions(game.board)

  piece_positions
  |> dict.fold(0, fn(eval, piece, positions) {
    // Number of times this piece type appears on the board. Piece types are
    // per-color, so two white knights -> length of two, regardless of how many
    // black knights there are.
    let count = list.length(positions)

    // This encodes negative numbers, so we subtract every time we see an enemy
    // piece
    let value = score.piece(piece)

    eval + value * count
  })
}
