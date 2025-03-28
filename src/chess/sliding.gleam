import chess/constants
import chess/piece.{type Piece, Bishop, King, Knight, Pawn, Queen, Rook}
import gleam/int

pub type SlidingDirection {
  Up
  Down
  Left
  Right
  UpLeft
  UpRight
  DownLeft
  DownRight
}

/// Return the maximum distance that a given piece can go
pub fn piece_distance(
  piece: Piece,
  dir: SlidingDirection,
) -> Result(Int, String) {
  case piece {
    // TODO: return an error here
    Knight(_) ->
      Error(
        "A knight doesn't slide! Use another function (currently unimplemented) to generate knight moves.",
      )
    Pawn(_) ->
      Error(
        "A pawn doesn't slide! Use another function (currently unimplemented) to generate pawn moves.",
      )

    // That was easy!
    King(_) -> 1 |> Ok

    Rook(_) -> rook_distance(dir) |> Ok
    Bishop(_) -> bishop_distance(dir) |> Ok
    Queen(_) -> queen_distance(dir) |> Ok
  }
}

fn rook_distance(dir: SlidingDirection) -> Int {
  // Subtract 1 since we're currently on a piece
  let horizontal_distance = constants.row_len - 1
  let vertical_distance = constants.col_len - 1

  case dir {
    Up | Down -> horizontal_distance
    Left | Right -> vertical_distance

    _ -> 0
  }
}

fn bishop_distance(dir: SlidingDirection) {
  // Subtract 1 since we're currently on a piece
  let horizontal_distance = constants.row_len - 1
  let vertical_distance = constants.col_len - 1

  case dir {
    UpLeft | UpRight | DownLeft | DownRight ->
      int.min(horizontal_distance, vertical_distance)

    _ -> 0
  }
}

fn queen_distance(dir: SlidingDirection) -> Int {
  case dir {
    Up | Down | Left | Right -> rook_distance(dir)
    _ -> bishop_distance(dir)
  }
}
