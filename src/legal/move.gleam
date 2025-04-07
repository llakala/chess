import chess/board.{type Board}
import chess/color.{type Color}
import chess/game.{type Game, Game}
import chess/position.{type Position}
import chess/square

pub type Move {
  Move(from: Position, to: Position)
  Capture(from: Position, to: Position)
  PassantMove(from: Position, to: Position)
  QueenCastle(from: Position, to: Position)
  KingCastle(from: Position, to: Position)
}

pub fn to_string(move: Move) -> String {
  let from_str = move.from |> position.to_string
  let to_str = move.to |> position.to_string
  let move_type = case move {
    Move(_, _) -> ""
    Capture(_, _) -> "Capture "
    PassantMove(_, _) -> "En Passant "
    QueenCastle(_, _) -> "Castle Queenside "
    KingCastle(_, _) -> "Castle Kingside "
  }

  move_type <> from_str <> " -> " <> to_str
}

/// Simple move function that moves a piece to another place on the board,
/// capturing if another piece is at the target square. This function
/// doesn't encode any logic about legal moves - you can make a piece go
/// anywhere with this as it stands.
/// I would put this in the `board` class, but then I get circular
/// dependency issues.
pub fn apply(game: Game, move: Move) -> Game {
  let board = game.board
  let new_board = case move {
    Move(_, _) -> apply_move(board, move)

    // We currently handle captures the same - but having them as Capture means
    // we can filter for them in a list of moves
    Capture(_, _) -> apply_move(board, move)

    QueenCastle(_, _) -> apply_queen_castle(board, move, game.color)
    KingCastle(_, _) -> apply_king_castle(board, move, game.color)

    PassantMove(_, _) -> apply_passant(board, move)
  }
  Game(..game, board: new_board)
}

fn apply_move(board: Board, move: Move) -> Board {
  let square = board.get_pos(board, move.from)
  board
  |> board.set_pos(move.from, square.None)
  |> board.set_pos(move.to, square)
}

fn apply_king_castle(_board: Board, _move: Move, _color: Color) -> Board {
  panic as "Unimplemented!"
}

fn apply_queen_castle(_board: Board, _move: Move, _color: Color) -> Board {
  panic as "Unimplemented!"
}

fn apply_passant(_board: Board, _move: Move) -> Board {
  panic as "Unimplemented!"
}
