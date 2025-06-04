import chess/board
import chess/game.{type Game}
import gleam/set.{type Set}
import legal/targets
import piece/square
import position/position.{type Position}

/// Given some game, return all the positions that the current player can
/// attack.
pub fn attacked_positions(game: Game) -> Set(Position) {
  // All the positions that have a piece belonging to the current player
  let positions = game.player_positions(game)

  // For each position that could attack, potentially update the data
  set.fold(positions, set.new(), fn(accum, origin) {
    // all the targets this position is attacking
    let assert Ok(targets) = targets.from_pos(game, origin)

    // All the positions we can attack, disregarding the move types
    let destinations =
      targets
      |> targets.get_destinations

    set.union(accum, destinations)
  })
}

/// Given some game, return all the positions that the enemy could attack - even
/// if that position currently has another enemy on it!
pub fn endangered_positions(game: Game) -> Set(Position) {
  // All the positions that have a piece belonging to the enemy
  let positions = game.enemy_positions(game)

  // For each enemy position, potentially update the data
  set.fold(positions, set.new(), fn(accum, origin) {
    let square = board.get_pos(game.board, origin)
    let assert Ok(piece) = square.to_piece(square)

    let targets = targets.endangered_from_pos(game, origin, piece)

    let destinations = targets |> targets.get_destinations

    set.union(accum, destinations)
  })
}
