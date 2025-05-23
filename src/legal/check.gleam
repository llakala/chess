import chess/board
import chess/constants
import chess/game.{type Game}
import gleam/list
import gleam/result
import gleam/string
import iv.{type Array}
import legal/targets
import piece/color
import piece/piece.{King, Piece}
import piece/square
import position/position.{type Position}
import utils/text

/// Given some game, return all the squares the current player could attack. We
/// represent this as an array of length 64, just like a Board, where the first
/// element represents the top left square, and the last element represents the
/// bottom right square. However, rather than storing Squares, it stores
/// booleans. So, to access some position, you just need to turn that position
/// into the proper index - `position.to_index` - then access that index from
/// this data - and you can see whether that square is attacked.
pub fn attacked_squares(game: Game) -> Array(Bool) {
  // This is an iv array, because we're going to use it for creating the
  // returned data later, so it's useful to have it in this form already!
  let indices = iv.range(0, constants.num_cols * constants.num_rows - 1)

  // This maps every position on the board to whether it's actually attacked. We
  // start out by having it map each position on the board to False -
  // representing no attacked positions. Just like the Board, this can be
  // thought of as a board, starting from the top left.
  let pos_is_attacked =
    indices
    |> iv.map(fn(_) { False })

  // All the positions that have a piece belonging to the current player
  let positions = game.player_positions(game)

  // For each position that could attack, potentially update the data
  list.fold(positions, pos_is_attacked, fn(data, origin) {
    // all the targets this position is attacking
    let assert Ok(targets) = targets.from_pos(game, origin)

    // All the positions we can attack, disregarding the move types
    let destinations = targets |> list.map(fn(target) { target.destination })

    // For each position we can attack, set it to True in the data
    list.fold(destinations, data, fn(data, pos) {
      let index = pos |> position.to_index
      let assert Ok(data) = data |> iv.set(index, True)

      data
    })
  })
}

/// Given some game, return all the positions that the current player can
/// attack.
pub fn attacked_positions(game: Game) -> List(Position) {
  // All the positions that have a piece belonging to the current player
  let positions = game.player_positions(game)

  // For each position that could attack, potentially update the data
  list.fold(positions, [], fn(accum, origin) {
    // all the targets this position is attacking
    let assert Ok(targets) = targets.from_pos(game, origin)

    // All the positions we can attack, disregarding the move types
    let destinations = targets |> list.map(fn(target) { target.destination })

    list.append(accum, destinations)
  })
}

/// Given some game, return all the positions that the enemy could attack - even
/// if that position currently has another enemy on it!
pub fn endangered_positions(game: Game) -> List(Position) {
  // All the positions that have a piece belonging to the other player
  let positions = game.enemy_positions(game)

  // For each enemy position, potentially update the data
  list.fold(positions, [], fn(accum, origin) {
    let square = board.get_pos(game.board, origin)
    let assert Ok(piece) = square.to_piece(square)

    // We need to see what WE would do on that square, since flipping an enemy
    // gives us a friend. This will let us see squares that currently hold an
    // enemy, but would be dangerous if we moved onto them
    let piece = piece |> piece.flip

    let targets = targets.from_pos_as_piece(game, origin, piece)
    let destinations = targets |> list.map(fn(target) { target.destination })

    list.append(accum, destinations)
  })
}

/// Check whether the player is currently in check, by seeing if the enemy could
/// attack the king's current square. Also returns the position of your king, if
/// it's actually on the board, so you can use it for other stuff without
/// searching again. Will return an error if there wasn't a king of your color
/// on the board - which happens in some tests.
pub fn is_in_check(game: Game) -> Result(#(Bool, Position), String) {
  let color = game.color
  let king = Piece(King, color)

  let enemy_game = game.Game(..game, color: color |> color.invert)
  // All the squares the enemy is attacking
  let attacked_squares = attacked_squares(enemy_game)

  use king_position <- result.try(
    game.board
    |> board.search(king)
    |> result.replace_error(
      "There was no "
      <> string.inspect(color)
      <> " king on the board, so couldn't determine whether we were in check!",
    ),
  )

  let index = position.to_index(king_position)

  // Return whether our current square is being attacked
  case iv.get(attacked_squares, index) {
    Ok(val) -> Ok(#(val, king_position))
    // Thinking of this as a logic error for now
    _ ->
      panic as "Got bad data of the wrong length from the attacked_squares function!"
  }
}

/// Given the data from `attacked_squares`, highlight all the attacked squares
/// on the game board.
pub fn display(game: Game, data: Array(Bool)) {
  game.board
  |> board.index_format(fn(square, index) {
    let assert Ok(is_attacked) = iv.get(data, index)
    let square_str = square |> square.to_string

    case is_attacked {
      False -> square_str
      True -> square_str |> text.color(text.Cyan)
    }
  })
}
