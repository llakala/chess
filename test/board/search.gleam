import birdie
import chess/board
import chess/color.{Black, White}
import chess/piece.{King, Pawn, Piece, Queen}
import chess/position
import gleam/string

pub fn white_queen_test() {
  let piece = Piece(Queen, White)
  let board = board.initial()

  let assert Ok(pos) = board |> board.search(piece)

  pos
  |> position.to_string
  |> birdie.snap("Expected a white queen to be found at d1!")
}

pub fn black_pawn_test() {
  let piece = Piece(Pawn, Black)
  let board = board.initial()

  let assert Ok(pos) = board |> board.search(piece)

  pos
  |> position.to_string
  |> birdie.snap("Expected the first black pawn to be found at a7!")
}

pub fn not_found_test() {
  let piece = Piece(King, White)
  // Initial board, but with the white king missing
  let assert Ok(board) =
    board.new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQ1BNR")

  let result = board |> board.search(piece)

  result
  |> string.inspect
  |> birdie.snap(
    "Expected to not find a white king on the board, since it wasn't there!",
  )
}
