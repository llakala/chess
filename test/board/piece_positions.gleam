import birdie
import chess/board
import gleam/dict.{type Dict}
import gleam/list
import gleam/string
import piece/piece.{type Piece}
import position/position.{type Position}

fn map_keys(
  dict: Dict(key, value),
  func: fn(key) -> new_key,
) -> Dict(new_key, value) {
  let fold_func = fn(dict, key, value) { dict.insert(dict, func(key), value) }
  dict.fold(dict, from: dict.new(), with: fold_func)
}

fn display_positions(dict: Dict(Piece, List(Position))) -> String {
  dict
  |> dict.map_values(fn(_, positions) {
    positions |> list.sort(position.compare) |> list.map(position.to_string)
  })
  |> map_keys(fn(key) { key |> piece.to_string })
  |> string.inspect
  |> string.split("#")
  |> string.join("\n")
}

pub fn initial_test() {
  board.initial()
  |> board.piece_positions
  |> display_positions
  |> birdie.snap(
    "Expected to see all the posiions of each piece type on the board!",
  )
}

pub fn no_pieces_test() {
  let assert Ok(board) = board.new("8/8/8/8/8/8/8/8")

  board
  |> board.piece_positions
  |> display_positions
  |> birdie.snap(
    "Expected to see no piece -> positions mappings, since the board was empty!",
  )
}

pub fn all_queens_test() {
  let assert Ok(board) =
    board.new(
      "qqqqqqqq/qqqqqqqq/qqqqqqqq/qqqqqqqq/QQQQQQQQ/QQQQQQQQ/QQQQQQQQ/QQQQQQQQ",
    )
  board
  |> board.piece_positions
  |> display_positions
  |> birdie.snap(
    "Expected to see a black queen on rows 5-8, and a white queen on rows 1-7.",
  )
}
