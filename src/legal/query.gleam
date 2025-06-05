import chess/board
import chess/game.{type Game}
import gleam/list
import gleam/set.{type Set}
import legal/targets
import piece/square
import position/change
import position/move.{type Move}
import position/position.{type Position}

/// Given some game, return all the positions that the enemy can
/// attack.
pub fn attacked_positions(game: Game) -> Set(Position) {
  // All the positions that have a piece belonging to the enemy
  let positions = game.enemy_positions(game)

  // For each enemy position, potentially update the data
  set.fold(positions, set.new(), fn(accum, origin) {
    // all the targets this position is attacking
    let assert Ok(targets) = targets.from_pos(game, origin)

    // All the positions the enemy can attack, disregarding the move types
    let destinations =
      targets
      |> targets.get_destinations

    set.union(accum, destinations)
  })
}

/// Given some game, return all the positions that the king cannot move to,
/// since they're being attacked by the enemy. These positions include squares
/// that aren't "actually" being attacked right now - but if we moved there,
/// they would be.
pub fn dangerous_destinations(game: Game) -> Set(Position) {
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

/// Addendum function to `dangerous_destinations`, for debugging, where you
/// actually want to see each piece's dangerous destination.
pub fn dangerous_moves(game: Game) -> List(Move) {
  // All the positions that have a piece belonging to the enemy
  let positions = game.enemy_positions(game)

  // For each enemy position, potentially update the data
  set.fold(positions, [], fn(accum, origin) {
    let square = board.get_pos(game.board, origin)
    let assert Ok(piece) = square.to_piece(square)

    let targets = targets.endangered_from_pos(game, origin, piece)

    let moves =
      list.map(targets, fn(target) {
        let change = change.Change(origin, target.destination)
        move.Move(change, target.kind)
      })

    list.append(moves, accum)
  })
}
