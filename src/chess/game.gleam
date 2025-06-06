import chess/board
import chess/castling.{type Castling}
import gleam/bool
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string
import iv
import piece/color.{type Color}
import piece/square
import position/direction.{type Direction}
import position/position.{type Position}

pub type Distance {
  NonCapture(distance: Int)
  Capture(distance: Int)
}

pub type Game {
  Game(
    board: board.Board,
    color: color.Color,
    castling: Castling,
    passant: Option(Position),
    halfmoves: Int,
    fullmoves: Int,
  )
}

pub fn initial() -> Game {
  // Okay with an assertion here, since if this went wrong, the logic failed.
  let assert Ok(output) =
    new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

  output
}

pub fn new(fen: String) -> Result(Game, String) {
  let split_fen = fen |> string.split(" ")

  let #(
    board_str,
    color_fen,
    castling_fen,
    passant_fen,
    halfmoves_fen,
    fullmoves_fen,
  ) = case split_fen {
    // Fen didn't come with halfmoves and fullmoves - assume they're 1
    [board, color, castling, passant] -> #(
      board,
      color,
      castling,
      passant,
      "1",
      "1",
    )

    [board, color, castling, passant, halfmoves, fullmoves] -> #(
      board,
      color,
      castling,
      passant,
      halfmoves,
      fullmoves,
    )

    _ ->
      panic as "Fen wasn't able to be parsed, since it had an amount of sections we didn't expect!"
  }

  use board <- result.try(board_str |> board.new)
  use color <- result.try(color_fen |> color.from_fen)
  use castling <- result.try(castling_fen |> castling.from_fen)
  use passant <- result.try(passant_fen |> parse_passant)
  use halfmoves <- result.try(halfmoves_fen |> parse_halfmoves)
  use fullmoves <- result.try(fullmoves_fen |> parse_fullmoves)

  Game(board, color, castling, passant, halfmoves, fullmoves) |> Ok
}

pub fn flip(game: Game) {
  Game(..game, color: game.color |> color.invert)
}

/// Does NOT turn it back into a fen string - simply displays for debugging
pub fn to_string(fen: Game) -> String {
  let board_str = fen.board |> board.to_string
  let color_str = fen.color |> string.inspect
  let castling_str = fen.castling |> castling.to_string
  let passant_str = fen.passant |> string.inspect
  let halfmoves_str = fen.halfmoves |> string.inspect
  let fullmoves_str = fen.fullmoves |> string.inspect

  "Board:\n"
  <> board_str
  <> "\n\nColor: "
  <> color_str
  <> "\n\nCastling:\n"
  <> castling_str
  <> "\n\nPassant: "
  <> passant_str
  <> "\n\nHalf moves: "
  <> halfmoves_str
  <> "\n\nFull moves: "
  <> fullmoves_str
}

/// Get all the positions that store a piece of the current player's color. A
/// game has a color as one of its values, since it's representative of whose
/// turn it is, so we use that to choose which color's pieces get returned.
pub fn player_positions(game: Game) -> Set(Position) {
  // An array of squares.
  let data = game.board |> board.get_data

  let my_color = game.color

  // For each square on the board, choose whether to add its position to the
  // set of friendly positions.
  iv.index_fold(data, set.new(), fn(positions, square, index) {
    case square {
      // Empty square - skip and keep iterating
      square.None -> positions

      // Square holds an enemy piece - skip
      square.Some(piece) if piece.color != my_color -> positions

      // Squares that hold a friendly piece :3
      square.Some(_) -> {
        // This only fails if the index is out of bounds, which it should never
        // be with a board unless I have a logic error.
        let assert Ok(pos) = position.from_index(index)

        set.insert(positions, pos)
      }
    }
  })
}

pub fn enemy_positions(game: Game) -> Set(Position) {
  // An array of squares.
  let data = game.board |> board.get_data

  let my_color = game.color

  // For each square on the board, choose whether to add its position to the
  // set of enemy positions.
  iv.index_fold(data, set.new(), fn(positions, square, index) {
    case square {
      // Empty square - skip and keep iterating
      square.None -> positions

      // Square holds a friendly piece - skip
      square.Some(piece) if piece.color == my_color -> positions

      // Squares that hold an enemy piece :o
      square.Some(_) -> {
        // This only fails if the index is out of bounds, which it should never
        // be with a board unless I have a logic error.
        let assert Ok(pos) = position.from_index(index)

        set.insert(positions, pos)
      }
    }
  })
}

/// Return the number of squares in a direction until you either bump into a wall
/// or hit another piece. Useful for determining the number of valid moves for
/// a piece in a direction. Returns a custom type Distance, so you can tell if there
/// was a capture in that direction. Note that we take Color rather than reusing
/// the game color, so we can "fib" about the color of a piece if necessary.
pub fn obstructed_distance(
  game game: Game,
  position position: Position,
  direction direction: Direction,
  color color: Color,
  captures_only captures_only: Bool,
) -> Distance {
  obstructed_distance_loop(game, position, direction, color, captures_only, 0)
}

/// We represent passant as an optional position, since sometimes a pawn hasn't
/// moved two spaces in one turn.
fn parse_passant(fen: String) -> Result(Option(Position), String) {
  use <- bool.guard(fen == "-", Ok(None))

  case position.new(fen) {
    Error(_) -> Error("Invalid passant string in fen!")
    Ok(pos) -> Some(pos) |> Ok
  }
}

fn obstructed_distance_loop(
  game: Game,
  pos: Position,
  dir: Direction,
  color: Color,
  go_past_obstructions: Bool,
  distance: Int,
) -> Distance {
  let board = game.board

  // Distance next position in the direction. If from_offset returns an error, we've
  // gone too far and gone off the board edge -- return the accumulated distance
  // immediately, without a capture since we never hit one
  case position.in_direction(pos, 1, dir) {
    Error(_) -> NonCapture(distance)

    Ok(new_pos) -> {
      let next_square = board.get_pos(board, new_pos)

      // Convert the square into a piece. If `to_piece` returns an error, the square
      // must've been empty. In that case, simply keep the loop going, adding 1 to
      // the accumulated distance
      case go_past_obstructions, square.to_piece(next_square) {
        // Piece at the next square is a friend - can't capture it. Note that
        // `distance` never gets modified, so this will be the distance to the
        // current square, not the new square.
        False, Ok(piece) if color == piece.color -> NonCapture(distance)

        // Other piece is an enemy and can be captured - increase the distance by
        // one, representing that we could capture the piece at the next square
        False, Ok(_) -> Capture(distance + 1)

        // If we see a friend, we still stop - but include the friend as a
        // potential move. This represents that the friend is still "in danger"
        // - if an enemy moved to its position to capture it, they would now be
        // in danger.
        True, Ok(piece) if color == piece.color -> NonCapture(distance + 1)

        // Either to_piece returned an error and the square is empty, or we
        // only care about captures and can skip this square if it's an enemy.
        _, _ ->
          obstructed_distance_loop(
            game,
            new_pos,
            dir,
            color,
            go_past_obstructions,
            distance + 1,
          )
      }
    }
  }
}

fn parse_halfmoves(fen: String) -> Result(Int, String) {
  case int.parse(fen) {
    Error(_) ->
      Error(
        "Expected halfmoves fen to be an integer, but found value `"
        <> fen
        <> "`.",
      )
    Ok(parsed) -> parsed |> Ok
  }
}

fn parse_fullmoves(fen: String) -> Result(Int, String) {
  case int.parse(fen) {
    Error(_) ->
      Error(
        "Expected fullmoves fen to be an integer, but found value `"
        <> fen
        <> "`.",
      )
    Ok(parsed) -> parsed |> Ok
  }
}
