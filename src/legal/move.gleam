import chess/board
import chess/game.{type Game, Game}
import chess/offset.{Offset}
import chess/piece.{type Piece}
import chess/position
import chess/square
import gleam/order.{type Order}
import gleam/string
import legal/change.{type Change}

pub type Move {
  Basic(change: Change)
  Capture(change: Change)
  Promotion(change: Change, new: piece.Piece)
  PromotionCapture(change: Change, new: piece.Piece)
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
    Promotion(_, piece) -> {
      let piece_name = piece.kind |> string.inspect
      piece_name <> " Promotion "
    }
    PromotionCapture(_, piece) -> {
      let piece_name = piece.kind |> string.inspect
      piece_name <> " Promotion & Capture "
    }
  }

  move_str <> change_str
}

/// Simple function that calls the correct `apply` function for the given
/// move. This lets en passant, queen castle, etc, be executed differently
/// when we're actually applying moves.
pub fn apply(game: Game, move: Move) -> Game {
  let change = move.change

  case move {
    Basic(_) -> change.apply(game, change)

    // We currently handle captures the same - but having them as Capture means
    // we can have special logic for them in other cases
    Capture(_) -> change.apply(game, change)

    QueenCastle(_) -> apply_queen_castle(game, change)
    KingCastle(_) -> apply_king_castle(game, change)

    Passant(_) -> apply_passant(game, change)
    Promotion(_, piece) | PromotionCapture(_, piece) ->
      apply_promotion(game, change, piece)
  }
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
  let game = change.apply(game, change)

  // Remove the enemy, thereby capturing it.
  Game(..game, board: board.set_pos(game.board, enemy_pos, square.None))
}

fn apply_promotion(game: Game, change: Change, piece: Piece) -> Game {
  // Move the original piece to the new position
  let game = change.apply(game, change)

  // Replace the moved piece with the piece it's promoting into
  let square = piece |> square.Some
  let board = game.board |> board.set_pos(change.to, square)

  // Replace the existing board with our new board
  Game(..game, board:)
}
