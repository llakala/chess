import bot/bot
import chess/color.{type Color, Black, White}
import chess/game
import gleam/dynamic/decode
import legal/generate
import legal/move

pub fn player_decoder() -> decode.Decoder(Color) {
  use player_string <- decode.then(decode.string)
  case player_string {
    "white" -> decode.success(White)
    "black" -> decode.success(Black)
    _ -> decode.failure(White, "Invalid player")
  }
}

pub fn move(
  fen: String,
  _turn: Color,
  _failed_moves: List(String),
) -> Result(String, String) {
  let assert Ok(game) = game.new(fen)
  let moves = generate.legal_moves(game)
  let move = bot.first(game, moves)
  let assert Ok(move_output) = move |> move.to_algebraic(game)

  Ok(move_output)
}
