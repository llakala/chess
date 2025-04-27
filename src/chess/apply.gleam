import chess/board
import chess/game.{type Game, Game}
import piece/square
import position/move.{
  type Move, Basic, KingCastle, Passant, Promotion, PromotionCapture,
  QueenCastle,
}
import position/position

import piece/piece.{type Piece}
import position/change.{type Change}
import position/offset.{Offset}

/// Apply some move to the current game, getting a new Game as the output.
pub fn move(game: Game, move: Move) -> Game {
  let change = move.change

  case move {
    Basic(_) -> apply_generic(game, change)

    // We currently handle captures the same - but having them as Capture means
    // we can have special logic for them in other cases. This can't be
    // unqualified, since we have something else with the name Capture in this
    // module!
    move.Capture(_) -> apply_generic(game, change)

    QueenCastle(_) -> apply_queen_castle(game, change)
    KingCastle(_) -> apply_king_castle(game, change)

    Passant(_) -> apply_passant(game, change)
    Promotion(_, piece) | PromotionCapture(_, piece) ->
      apply_promotion(game, change, piece)
  }
}

/// Apply some change to the board directly, moving the value at one position to some
/// other position. Note that this is currently not checked to ensure that
/// `change.from` is non-empty - be careful!
fn apply_generic(game: Game, change: Change) -> Game {
  let square = board.get_pos(game.board, change.from)
  let new_board =
    game.board
    |> board.set_pos(change.from, square.None)
    |> board.set_pos(change.to, square)

  // Update the game with the new board
  Game(..game, board: new_board)
}

fn apply_king_castle(_game: Game, _change: Change) -> Game {
  panic as "Unimplemented!"
}

fn apply_queen_castle(_game: Game, _change: Change) -> Game {
  panic as "Unimplemented!"
}

fn apply_passant(game: Game, change: Change) -> Game {
  // The change between the two positions
  let offset = change |> change.to_offset
  // Get the horizontal piece of the offsets - en passant moves you horizontally
  // towards the enemy piece, so grabbing the horizontal part lets us get the
  // position of the captured enemy.
  let assert Ok(enemy_pos) =
    Offset(0, offset.horizontal) |> position.apply_offset(change.from, _)

  // Move our piece diagonally to the new position
  let game = apply_generic(game, change)

  // Remove the enemy, thereby capturing it.
  Game(..game, board: board.set_pos(game.board, enemy_pos, square.None))
}

fn apply_promotion(game: Game, change: Change, piece: Piece) -> Game {
  // Move the original piece to the new position
  let game = apply_generic(game, change)

  // Replace the moved piece with the piece it's promoting into
  let square = piece |> square.Some
  let board = game.board |> board.set_pos(change.to, square)

  // Replace the existing board with our new board
  Game(..game, board:)
}
