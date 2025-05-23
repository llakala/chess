import bot/bot
import chess/algebraic
import chess/game
import gleam/dynamic/decode
import piece/color.{type Color, Black, White}

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
  let move = bot.minimax(game)
  let assert Ok(move_output) = move |> algebraic.notation(game)

  Ok(move_output)
}
