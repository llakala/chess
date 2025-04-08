import chess/board.{type Board}
import chess/position.{type Position}
import chess/square
import gleam/order.{type Order}

pub type Move {
  Move(from: Position, to: Position)
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
