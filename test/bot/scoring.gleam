import birdie
import bot/score
import chess/game
import gleam/list
import gleam/string
import legal/generate
import position/move

pub fn score_test() {
  let assert Ok(game) =
    game.new("rnbk1Bn1/p1pp1ppr/4p2p/1N2q3/2P5/3P4/P3PPPP/R2QKBNR b KQ - 2 12")

  let moves = generate.legal_moves(game)

  let first_part =
    moves
    |> list.sort(move.compare)
    |> list.map(fn(move) { move |> move.to_string })
    |> string.inspect

  let second_part =
    moves
    |> list.sort(fn(first, second) { score.compare_moves(game, first, second) })
    |> list.map(move.to_string)
    |> string.inspect

  { first_part <> "\n\n" <> second_part }
  |> birdie.snap("Before and after sorting moves by their score!")
}
