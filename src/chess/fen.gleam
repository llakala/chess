import chess/board
import chess/castling.{type Castling}
import chess/color
import chess/position.{type Position}
import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type Fen {
  Fen(
    data: board.Board,
    color: color.Color,
    castling: Castling,
    passant: Option(Position),
    halfmoves: Int,
    fullmoves: Int,
  )
}

pub fn initial() -> Fen {
  // Okay with an assertion here, since if this went wrong, the logic failed.
  let assert Ok(output) =
    new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

  output
}

pub fn new(fen: String) -> Result(Fen, String) {
  let split_fen = fen |> string.split(" ")

  use <- bool.guard(
    split_fen |> list.length != 6,
    Error("Invalid fen string with parts not equal to 6!"),
  )

  // Let assert is fine, we just had a guard clause
  let assert [
    board_str,
    color_fen,
    castling_fen,
    passant_fen,
    halfmoves_fen,
    fullmoves_fen,
  ] = split_fen

  use board <- result.try(board_str |> board.from_fen)
  use color <- result.try(color_fen |> color.from_fen)
  use castling <- result.try(castling_fen |> castling.from_fen)
  use passant <- result.try(passant_fen |> parse_passant)
  use halfmoves <- result.try(halfmoves_fen |> parse_halfmoves)
  use fullmoves <- result.try(fullmoves_fen |> parse_fullmoves)

  Fen(board, color, castling, passant, halfmoves, fullmoves) |> Ok
}

/// Does NOT turn it back into a fen string - simply displays for debugging
pub fn to_string(fen: Fen) {
  let board_str = fen.data |> board.to_string
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

/// We represent passant as an optional position, since sometimes a pawn hasn't
/// moved two spaces in one turn.
fn parse_passant(fen: String) -> Result(Option(Position), String) {
  use <- bool.guard(fen == "-", Ok(None))

  case position.new(fen) {
    Error(_) -> Error("Invalid passant string in fen!")
    Ok(pos) -> Some(pos) |> Ok
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
