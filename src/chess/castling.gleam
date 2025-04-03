import chess/choose
import gleam/list
import gleam/result
import gleam/string

pub opaque type Castling {
  Castling(
    white_queen: Bool,
    white_king: Bool,
    black_queen: Bool,
    black_king: Bool,
  )
}

/// Prints an output to show which directions you can castle. For example, given a
/// castling instance that can still castle in every direction, it would output:
///
/// ```
/// "White: King, Queen
/// Black: King, Queen"
/// ```
pub fn to_string(castling: Castling) {
  let black_king = case castling.black_king {
    False -> ""
    True -> " King"
  }

  let black_queen = case castling.black_queen {
    False -> ""
    True -> " Queen"
  }

  let white_king = case castling.white_king {
    False -> ""
    True -> " King"
  }

  let white_queen = case castling.white_queen {
    False -> ""
    True -> " Queen"
  }

  let white_status =
    [white_king, white_queen]
    |> join(" and")
    |> string.append("White:", _)

  let black_status =
    [black_king, black_queen]
    |> join(" and")
    |> string.append("Black:", _)

  [white_status, black_status]
  |> string.join("\n")
  |> string.append("Directions able to castle:\n", _)
}

pub fn from_fen(fen: String) -> Result(Castling, String) {
  let empty_castling = Castling(False, False, False, False)

  // If the string is empty, we can't castle at all!
  use #(current, rest) <- choose.cases(
    string.pop_grapheme(fen),
    on_error: fn(_) { empty_castling |> Ok },
  )

  // Start the loop without anything able to castle - we enable them as we go!
  from_fen_loop(current, rest, empty_castling)
}

fn from_fen_loop(
  current: String,
  rest: String,
  castling: Castling,
) -> Result(Castling, String) {
  // Parse the current character for which direction we can castle. If it's some
  // other letter, the string was invalid
  use castling <- result.try(case current {
    "K" -> Castling(..castling, white_king: True) |> Ok
    "k" -> Castling(..castling, black_king: True) |> Ok
    "Q" -> Castling(..castling, white_queen: True) |> Ok
    "q" -> Castling(..castling, black_queen: True) |> Ok
    _ -> Error("Current letter `" <> current <> "` invalid! Not in `kKqQ`.")
  })

  case rest |> string.pop_grapheme {
    // pop_grapheme failed, meaning string ended. We're done looping, return
    // Castling!
    Error(_) -> Ok(castling)

    // Keep looping with the next letter in the string
    Ok(#(current, rest)) -> from_fen_loop(current, rest, castling)
  }
}

// Custom join function that doesn't add the separator if list is empty
fn join(list: List(String), with: String) -> String {
  // All the strings in the list concatenated together
  let all_contents = list.fold(list, "", string.append)

  case all_contents {
    "" -> ""
    _ -> string.join(list, with)
  }
}
