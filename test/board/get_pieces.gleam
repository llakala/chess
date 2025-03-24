import birdie
import chess/position
import iv

import chess/board
import chess/color
import chess/square
import gleam/string

pub fn get_pieces_test() {
  let board = board.initial()
  let color = color.White

  let my_squares =
    board
    |> board.get_pieces(color)

  // Just getting the pieces - we could access the positions
  let my_pieces = my_squares |> iv.map(square.to_piece)

  let my_positions = my_squares |> iv.map(square.to_position)

  my_pieces
  |> iv.to_list
  |> string.inspect
  |> birdie.snap("All the white pieces!")

  my_positions
  |> iv.map(position.to_algebraic)
  |> iv.to_list
  |> string.inspect
  |> birdie.snap("All the positions of the white pieces!")
}
