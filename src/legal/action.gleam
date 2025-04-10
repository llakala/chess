import chess/board.{type Board}
import chess/color.{type Color}
import chess/game.{type Game, Game}
import gleam/order.{type Order, Gt, Lt}
import legal/move.{type Move}

pub type Action {
  Basic(move: Move)
  Capture(move: Move)
  Passant(move: Move)
  QueenCastle(move: Move)
  KingCastle(move: Move)
}

pub fn compare(first: Action, second: Action) -> Order {
  case first, second {
    Basic(_), Basic(_) -> move.compare(first.move, second.move)
    Basic(_), _ -> Lt
    _, Basic(_) -> Gt
    _, _ -> move.compare(first.move, second.move)
  }
}

pub fn to_string(action: Action) -> String {
  let move_str = action.move |> move.to_string
  let action_str = case action {
    Basic(_) -> ""
    Capture(_) -> "Capture "
    Passant(_) -> "En Passant "
    QueenCastle(_) -> "Castle Queenside "
    KingCastle(_) -> "Castle Kingside "
  }

  action_str <> move_str
}

/// Simple function that calls the correct `apply` function for the given
/// action. This lets en passant, queen castle, etc, be executed differently
/// when we're actually applying moves.
pub fn apply(game: Game, action: Action) -> Game {
  let board = game.board
  let move = action.move
  let new_board = case action {
    Basic(_) -> move.apply(board, move)

    // We currently handle captures the same - but having them as Capture means
    // we can filter for them in a list of moves
    Capture(_) -> move.apply(board, move)

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
