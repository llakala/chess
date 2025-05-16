import chess/board
import chess/game.{type Game}
import gleam/dict
import gleam/list
import piece/color.{type Color}
import piece/piece.{type Piece}
import position/move.{type Move}

const pawn_value = 1

const knight_value = 3

const bishop_value = 3

const rook_value = 5

const queen_value = 9

// I should probably eventually give this a value, to de-incentivize check so
// we end up not even needing check logic for move application - but not
// bothering with it for now.
const king_value = 0

/// This type isn't used within the `eval` module - instead, you should apply
/// the moves yourself, and see what score they result in.
pub type EvaluatedMove {
  EvaluatedMove(move: Move, score: Int)
}

/// Given some game state, return an evaluation score.
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
    let value = piece_value(piece, game.color)

    eval + value * count
  })
}

/// Returns the value of some piece - will be a negative number if the piece is
/// an enemy.
pub fn piece_value(piece: Piece, player_color: Color) {
  let value = case piece.kind {
    piece.Bishop -> bishop_value
    piece.King -> king_value
    piece.Knight -> knight_value
    piece.Pawn -> pawn_value
    piece.Queen -> queen_value
    piece.Rook -> rook_value
  }

  // Return a negative value if the piece is of the other color
  let sign = case player_color == piece.color {
    True -> 1
    False -> -1
  }

  sign * value
}
