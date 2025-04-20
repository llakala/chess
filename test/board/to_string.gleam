import birdie
import chess/board

pub fn to_string_empty_test() {
  board.empty() |> board.to_string |> birdie.snap(title: "Empty board")
}

pub fn to_string_full_board_test() {
  let assert Ok(board) =
    board.new(
      "PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP",
    )

  board
  |> board.to_string
  |> birdie.snap(title: "Board of only pawns")
}
