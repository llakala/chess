import chess/board.{type Board}
import chess/color.{type Color}
import chess/game.{type Game, Game}
import chess/offset.{Offset}
import chess/position
import chess/square
import gleam/order.{type Order}
import legal/change.{type Change}

pub type Move {
  Basic(change: Change)
  Capture(change: Change)
  Passant(change: Change)
  QueenCastle(change: Change)
  KingCastle(change: Change)
}

pub fn compare(first: Move, second: Move) -> Order {
  change.compare(first.change, second.change)
}

pub fn to_string(move: Move) -> String {
  let change_str = move.change |> change.to_string
  let move_str = case move {
    Basic(_) -> ""
    Capture(_) -> "Capture "
    Passant(_) -> "En Passant "
    QueenCastle(_) -> "Castle Queenside "
    KingCastle(_) -> "Castle Kingside "
  }

  move_str <> change_str
}

/// Simple function that calls the correct `apply` function for the given
/// move. This lets en passant, queen castle, etc, be executed differently
/// when we're actually applying moves.
pub fn apply(game: Game, move: Move) -> Game {
  let board = game.board
  let change = move.change
  let color = game.color

  let new_board = case move {
    Basic(_) -> change.apply(board, change)

    // We currently handle captures the same - but having them as Capture means
    // we can filter for them in a list of moves
    Capture(_) -> change.apply(board, change)

    QueenCastle(_) -> apply_queen_castle(board, change, color)
    KingCastle(_) -> apply_king_castle(board, change, color)

    Passant(_) -> apply_passant(board, change)
  }
  Game(..game, board: new_board)
}

fn apply_king_castle(_board: Board, _change: Change, _color: Color) -> Board {
  panic as "Unimplemented!"
}

fn apply_queen_castle(_board: Board, _change: Change, _color: Color) -> Board {
  panic as "Unimplemented!"
}

fn apply_passant(board: Board, change: Change) -> Board {
  // The change between the two positions
  let offset = change |> change.to_offset
  // Get the horizontal piece of the offsets - en passant moves you horizontally
  // towards the enemy piece, so grabbing the horizontal part lets us get the
  // position of the captured enemy.
  let assert Ok(enemy_pos) =
    Offset(0, offset.horizontal) |> position.apply_offset(change.from, _)

  // Move our piece diagonally to the new position
  change.apply(board, change)
  // Remove the enemy, thereby capturing it.
  |> board.set_pos(enemy_pos, square.None)
}
