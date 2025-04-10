import chess/board.{type Board}
import chess/position.{type Position}
import chess/square
import gleam/order.{type Order}

pub type Change {
  Change(from: Position, to: Position)
}

pub fn compare(first: Change, second: Change) -> Order {
  // First try to sort on `from` - if you get `Eq`, then sort on `to`. Only if that
  // ALSO returns Eq do we return Eq.
  use <- order.lazy_break_tie(position.compare(first.from, second.from))
  position.compare(first.to, second.to)
}

pub fn apply(board: Board, change: Change) -> Board {
  let square = board.get_pos(board, change.from)
  board
  |> board.set_pos(change.from, square.None)
  |> board.set_pos(change.to, square)
}

pub fn to_string(change: Change) {
  let from_str = change.from |> position.to_string
  let to_str = change.to |> position.to_string

  from_str <> " -> " <> to_str
}
