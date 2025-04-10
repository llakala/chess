import chess/board.{type Board}
import chess/color.{type Color}
import chess/game.{type Game, Game}
import gleam/order.{type Order, Gt, Lt}
import legal/change.{type Change}

pub type Action {
  Basic(change: Change)
  Capture(change: Change)
  Passant(change: Change)
  QueenCastle(change: Change)
  KingCastle(change: Change)
}

pub fn compare(first: Action, second: Action) -> Order {
  case first, second {
    Basic(_), Basic(_) -> change.compare(first.change, second.change)
    Basic(_), _ -> Lt
    _, Basic(_) -> Gt
    _, _ -> change.compare(first.change, second.change)
  }
}

pub fn to_string(action: Action) -> String {
  let change_str = action.change |> change.to_string
  let action_str = case action {
    Basic(_) -> ""
    Capture(_) -> "Capture "
    Passant(_) -> "En Passant "
    QueenCastle(_) -> "Castle Queenside "
    KingCastle(_) -> "Castle Kingside "
  }

  action_str <> change_str
}

/// Simple function that calls the correct `apply` function for the given
/// action. This lets en passant, queen castle, etc, be executed differently
/// when we're actually applying moves.
pub fn apply(game: Game, action: Action) -> Game {
  let board = game.board
  let change = action.change
  let new_board = case action {
    Basic(_) -> change.apply(board, change)

    // We currently handle captures the same - but having them as Capture means
    // we can filter for them in a list of moves
    Capture(_) -> change.apply(board, change)

    QueenCastle(_) -> apply_queen_castle(board, action, game.color)
    KingCastle(_) -> apply_king_castle(board, action, game.color)

    Passant(_) -> apply_passant(board, action)
  }
  Game(..game, board: new_board)
}

fn apply_king_castle(_board: Board, _action: Action, _color: Color) -> Board {
  panic as "Unimplemented!"
}

fn apply_queen_castle(_board: Board, _action: Action, _color: Color) -> Board {
  panic as "Unimplemented!"
}

fn apply_passant(_board: Board, _action: Action) -> Board {
  panic as "Unimplemented!"
}
