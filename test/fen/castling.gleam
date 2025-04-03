import birdie
import chess/castling

pub fn all_castle_test() {
  let str = "kKqQ"
  let assert Ok(castling_status) = str |> castling.from_fen
  castling_status
  |> castling.to_string
  |> birdie.snap("Able to castle on every side!")
}

pub fn no_castle_test() {
  let str = ""
  let assert Ok(castling_status) = str |> castling.from_fen
  castling_status
  |> castling.to_string
  |> birdie.snap("Not able to castle at all!")
}

pub fn white_castle_test() {
  let str = "KQ"
  let assert Ok(castling_status) = str |> castling.from_fen
  castling_status
  |> castling.to_string
  |> birdie.snap("Only white pieces can castle!")
}

pub fn black_castle_test() {
  let str = "kq"
  let assert Ok(castling_status) = str |> castling.from_fen
  castling_status
  |> castling.to_string
  |> birdie.snap("Only black pieces can castle!")
}
