import chess/board
import chess/change.{type Change}
import chess/game.{type Game}
import chess/piece
import chess/position
import chess/square
import gleam/list
import gleam/order.{type Order}
import gleam/string
import utils/text

pub type Move {
  Move(change: Change, kind: MoveKind)
}

pub type MoveKind {
  Basic
  Capture
  Promotion(new: piece.Piece)
  PromotionCapture(new: piece.Piece)
  Passant
  QueenCastle
  KingCastle
}

pub fn compare(first: Move, second: Move) -> Order {
  change.compare(first.change, second.change)
}

pub fn to_string(move: Move) -> String {
  let change_str = move.change |> change.to_string
  let kind_str = case move.kind {
    Basic -> ""
    Capture -> "Capture "
    Passant -> "En Passant "
    QueenCastle -> "Castle Queenside "
    KingCastle -> "Castle Kingside "
    Promotion(piece) -> {
      let piece_name = piece.kind |> string.inspect
      piece_name <> " Promotion "
    }
    PromotionCapture(piece) -> {
      let piece_name = piece.kind |> string.inspect
      piece_name <> " Promotion & Capture "
    }
  }

  kind_str <> change_str
}

/// Given a list of moves, format/sort the list, and show the origins and
/// destinations of the moves on the board.
pub fn display(moves: List(Move), game: Game) -> String {
  let origins = moves |> list.map(fn(move) { move.change.from })
  let destinations = moves |> list.map(fn(move) { move.change.to })

  // Takes a square and an index, and colors in the origins and destinations of
  // the moves.
  let colorize_square = fn(square, index) {
    let assert Ok(pos) = index |> position.from_index
    let square_str = square |> square.to_string

    case list.contains(origins, pos), list.contains(destinations, pos) {
      // Neither an origin or a destination
      False, False -> square_str

      // Origin, not a destination
      True, False -> square_str |> text.color(text.Cyan)

      // Destination, not an origin
      False, True -> square_str |> text.color(text.Yellow)

      // Both an origin and a destination
      True, True -> square_str |> text.color(text.Red)
    }
  }
  let moves_output =
    moves
    |> list.sort(compare)
    |> list.map(fn(move) { move |> to_string })
    |> string.inspect

  let board_output = board.index_format(game.board, colorize_square)

  moves_output <> "\nBoard:\n" <> board_output
}
