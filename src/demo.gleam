import bot/bot
import chess/game
import gleam/io
import gleam/list
import legal/generate

// Most of these are here because they fail with the existing logic (probably
// because of check)
const fens = [
  // "7k/p7/1R5K/6r1/6p1/6P1/8/8 w - - 0 1",
  // "R7/P4k2/8/8/8/8/r7/6K1 w - - 0 1",
  // "1k6/5RP1/1P6/1K6/6r1/8/8/8 w - - 0 1",
  "2b3k1/4rrpp/p2p4/2pP2RQ/1pP1Pp1N/1P3P1P/1q6/6RK w - - 0 1",
  // "2r5/1r6/4pNpk/3pP1qp/8/2P1QP2/5PK1/R7 w - - 0 1",
// "2Q2n2/2R4p/1p1qpp1k/8/3P3P/3B2P1/5PK1/r7 w - - 0 1",
// "rnbqkb1r/1p3ppp/5N2/1p2p1B1/2P5/8/PP2PPPP/R2QKB1R b KQkq - 0 1",
// "4N2k/5rpp/1Q6/p3q3/8/P5P1/1P3P1P/5K2 w - - 0 1",
// "2rq1bk1/p4p1p/1p4p1/3b4/3B1Q2/8/P4PpP/3RR1K1 w - - 0 1",
// "6kr/1q2r1p1/1p2N1Q1/5p2/1P1p4/6R1/7P/2R3K1 w - - 0 1",
// "4r1k1/p1qr1p2/2pb1Bp1/1p5p/3P1n1R/1B3P2/PP3PK1/2Q4R w - - 0 1",
]

pub fn main() {
  list.map(fens, fn(fen) {
    // We're recreating the logic of `chess.move` here to actually display the
    // move and board for testing
    let assert Ok(game) = game.new(fen)
    let move = bot.first(game)
    generate.display([move], game) |> io.println
  })
}
