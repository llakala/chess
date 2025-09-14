import chess/board
import chess/change.{type Change}
import chess/color.{Black, White}
import chess/game.{type Game, Game}
import chess/move.{
  type Move, Basic, KingCastle, Passant, Promotion, PromotionCapture,
  QueenCastle,
}
import chess/offset.{Offset}
import chess/piece.{type Piece}
import chess/position
import chess/square
import gleam/bool
import gleam/result

/// Apply some move to the current game, getting a new Game as the output.
/// Returns an error if the Change tried to start from a None position, or
/// somehow took you off the board. Listen, I'm just trying to cut down on my
/// `let assert` usage - don't judge me.
pub fn move(game: Game, move: Move) -> Result(Game, String) {
  let change = move.change

  case move.kind {
    Basic -> apply_generic(game, change)

    // We currently handle captures the same - but having them as Capture means
    // we can have special logic for them in other cases. This can't be
    // unqualified, since we have something else with the name Capture in this
    // module!
    move.Capture -> apply_generic(game, change)

    QueenCastle -> apply_queen_castle(game, change)
    KingCastle -> apply_king_castle(game, change)

    Passant -> apply_passant(game, change)
    Promotion(piece) | PromotionCapture(piece) ->
      apply_promotion(game, change, piece)
  }
}

/// Apply some change to the board directly, moving the value at one position to some
/// other position. We also  increase the number of moves. Note that this is currently
/// not checked to ensure that `change.from` is non-empty - be careful!
fn apply_generic(game: Game, change: Change) -> Result(Game, String) {
  let square = board.get_pos(game.board, change.from)
  use <- bool.guard(
    square == square.None,
    Error("The `from` part of the change was found to be empty!"),
  )

  let new_board =
    game.board
    |> board.set_pos(change.from, square.None)
    |> board.set_pos(change.to, square)

  // If it's black's turn, and white started, `halfmoves % 2 == 0` - increment
  // fullmoves!
  let fullmoves = case game.color {
    White -> game.fullmoves
    Black -> game.fullmoves + 1
  }

  // Update the game with the new board, and increment the
  // halfmoves and fullmoves
  Game(..game, board: new_board, halfmoves: game.halfmoves + 1, fullmoves:)
  |> Ok
}

fn apply_king_castle(_game: Game, _change: Change) -> Result(Game, String) {
  panic as "Unimplemented!"
}

fn apply_queen_castle(_game: Game, _change: Change) -> Result(Game, String) {
  panic as "Unimplemented!"
}

fn apply_passant(game: Game, change: Change) -> Result(Game, String) {
  // The change between the two positions
  let offset = change |> change.to_offset
  // en passant moves you horizontally towards the enemy piece, so grabbing the
  // horizontal part of the offset lets us get the position of the captured enemy.
  let horizontal_offset = Offset(vertical: 0, horizontal: offset.horizontal)

  case position.apply_offset(change.from, horizontal_offset) {
    Error(_) ->
      Error("Horizontal offset of the change somehow took you off the board!")
    Ok(enemy_pos) -> {
      // Move our piece diagonally to the new position - exit early if this fails
      use game <- result.try(apply_generic(game, change))

      // Remove the enemy, thereby capturing it.
      Game(..game, board: game.board |> board.set_pos(enemy_pos, square.None))
      |> Ok
    }
  }
}

fn apply_promotion(
  game: Game,
  change: Change,
  piece: Piece,
) -> Result(Game, String) {
  // Move the original piece to the new position
  use game <- result.try(apply_generic(game, change))

  // Replace the moved piece with the piece it's promoting into
  let square = piece |> square.Some
  let board = game.board |> board.set_pos(change.to, square)

  // Replace the existing board with our new board
  Game(..game, board:)
  |> Ok
}
