import chess/board
import chess/constants
import chess/game.{type Game}
import gleam/list
import gleam/result
import gleam/set
import gleam/string
import iv.{type Array}
import legal/apply
import legal/query
import legal/targets.{Target}
import legal/tarmap
import piece/color
import piece/piece.{King, Piece}
import piece/square
import position/change
import position/move.{type Move}
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
  set.fold(positions, pos_is_attacked, fn(data, origin) {
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

pub fn filter_tarmaps(
  pseudolegal_tarmaps: tarmap.TarmapCollection,
  game: Game,
) -> tarmap.TarmapCollection {
  case is_in_check(game) {
    // There's no king of our color on the board - no need to filter at all
    // for tarmaps putting us in check!
    Error(_) -> pseudolegal_tarmaps

    // Currently in check - filter out the tarmaps that would keep us in check
    Ok(#(True, king_pos)) ->
      filter_in_check(pseudolegal_tarmaps, game, king_pos)

    // Not currently in check - we need to filter out the moves that would put us
    // into check
    Ok(#(False, king_pos)) ->
      filter_not_in_check(pseudolegal_tarmaps, game, king_pos)
  }
}

fn filter_in_check(
  pseudolegal_tarmaps: tarmap.TarmapCollection,
  game: Game,
  king_pos: Position,
) -> tarmap.TarmapCollection {
  // This will basically be any square that has a direct line of sight to
  // the king. We need to find these to see if the king can move directly
  // out of check, or if some other piece can step into the line of fire for
  // it.
  let line_of_fire_positions = targets.empty_to_pos(game, king_pos)

  // Positions of enemies directly attacking the king. It might be possible
  // that a friendly piece could take the assassin and stop the threat on
  // the king.
  let enemy_positions = targets.enemies_to_pos(game, king_pos)

  // Most moves will be illegal, since we're in check. The only legal moves
  // are the ones made by the king, the ones that step into the line of fire
  // FOR the king, or the moves that kill the enemy attacking the king
  let potentially_legal_tarmaps =
    pseudolegal_tarmaps
    |> tarmap.filter(fn(_, target) {
      let Target(destination, _) = target
      set.contains(line_of_fire_positions, destination)
      || set.contains(enemy_positions, destination)
    })

  // Apply every target to see which ones take us out of check. Expensive -
  // so we try to run this on as few tarmaps as possible.
  let legal_tarmaps =
    tarmap.filter(potentially_legal_tarmaps, fn(origin, target) {
      let Target(destination, kind) = target

      let change = change.Change(origin, destination)
      let move = move.Move(change, kind)

      is_move_legal(move, game)
    })

  legal_tarmaps
}

fn filter_not_in_check(
  pseudolegal_tarmaps: tarmap.TarmapCollection,
  game: Game,
  king_pos: Position,
) -> tarmap.TarmapCollection {
  // The positions containing a friendly piece, that the king has a direct line
  // of sight to. This is important to find, since these might be defending the
  // king from check.
  // from check.
  let checkable_origins = targets.friends_to_pos(game, king_pos)

  // All the positions that could be attacked by the enemy - even if it
  // currently contains a friend. Ideally, this would only contain positions in
  // the same row, column, or diagonal as the king - something to look into.
  let attacked_positions = query.endangered_positions(game)

  let #(potentially_illegal, legal) =
    // Any value that returns True needs to be checked further. Anything
    // returning False is definitely legal
    tarmap.partition(pseudolegal_tarmaps, fn(origin, target) {
      let Target(destination, _) = target

      let interfering_friend =
        set.contains(checkable_origins, origin)
        && set.contains(attacked_positions, origin)

      let dangerous_king =
        origin == king_pos && set.contains(attacked_positions, destination)

      dangerous_king || interfering_friend
    })

  // Apply every target of each of these tarmaps to see which ones don't put us
  // in check. Expensive - so we try to run this on as few tarmaps as possible
  let filtered =
    tarmap.filter(potentially_illegal, fn(origin, target) {
      let Target(destination, kind) = target

      let change = change.Change(origin, destination)
      let move = move.Move(change, kind)

      is_move_legal(move, game)
    })

  tarmap.merge(legal, filtered)
}

pub fn is_move_legal(move: Move, game: Game) -> Bool {
  case apply.move(game, move) {
    Error(_) -> False
    Ok(new_game) ->
      // If this gives us an error, there was no king of our color on the
      // board. Crazy, I know - it happens in some tests, though.
      case is_in_check(new_game) {
        // Ignore the error and return True - meaning the move is safe.
        Error(_) -> True
        // Flip the result we get - we're using `list.filter`, which only
        // keeps the values that were True. So, if we were in check after that
        // move, return False, and filter out the move.
        Ok(#(is_in_check, _)) -> !is_in_check
      }
  }
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
