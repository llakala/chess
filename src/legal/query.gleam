import chess/board
import chess/game.{type Game}
import gleam/set.{type Set}
import legal/targets
import piece/piece
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
    let assert Ok(enemy_piece) = square.to_piece(square)

    // We're going through enemies, but sometimes, an enemy square is in danger,
    // as it's being attacked by another enemy - so we couln't move there. To
    // gather these positions, we need to see what targets a FRIENDLY piece at
    // the enemy square would see - so we simulate twice, once as an enemy, and
    // once as a friend
    let friendly_piece = enemy_piece |> piece.flip

    // enemy targets correspond to the targets that an enemy sees - which will
    // be empty squares and friendly squares. Likewise, friendly targets
    // correspond to the targets that a friend sees - which will be empty
    // squares and enemy targets.
    let enemy_targets = targets.from_pos_as_piece(game, origin, enemy_piece)
    let friendly_targets =
      targets.from_pos_as_piece(game, origin, friendly_piece)

    let friendly_destinations = friendly_targets |> targets.get_destinations
    let enemy_destinations = enemy_targets |> targets.get_destinations

    // We'll have duplicates between the two, because both will see empty
    // squares - thankfully, sets only have unique elements, so it's not a
    // problem!
    let destinations = set.union(friendly_destinations, enemy_destinations)

    set.union(accum, destinations)
  })
}
