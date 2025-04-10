import chess/board.{type Board}
import chess/position.{type Position}
import chess/square
import gleam/order.{type Order}

pub type Move {
  Move(from: Position, to: Position)
}

pub fn compare(first: Move, second: Move) -> Order {
  // First try to sort on `from` - if you get `Eq`, then sort on `to`. Only if that
  // ALSO returns Eq do we return Eq.
  use <- order.lazy_break_tie(position.compare(first.from, second.from))
  position.compare(first.to, second.to)
}

pub fn apply(board: Board, move: Move) -> Board {
  let square = board.get_pos(board, move.from)
  board
  |> board.set_pos(move.from, square.None)
  |> board.set_pos(move.to, square)
}

pub fn to_string(move: Move) {
  let from_str = move.from |> position.to_string
  let to_str = move.to |> position.to_string

  from_str <> " -> " <> to_str
}
