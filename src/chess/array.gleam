import gleam/bool
import gleam/result
import gleam/string
import iv.{type Array}

/// QOL debug function, to turn the Array into a list, print it to stderr, and return it unchanged
pub fn debug(arr: Array(a)) {
  arr |> iv.to_list |> echo |> iv.from_list
}

/// Split an iv array into `n` evenly-sized chunks.
/// Returns an error if the array can't be evenly split into chunks
pub fn sized_chunk(
  arr: Array(String),
  num_chunks: Int,
) -> Result(Array(Array(String)), String) {
  let len = iv.length(arr)
  use <- bool.guard(
    len % num_chunks != 0,
    Error("Number of splits doesn't go into the string's length"),
  )

  let contents = Ok(arr)
  let current_chunk = iv.new()
  let chunk_len = len / num_chunks
  let acc = iv.new()
  sized_chunk_loop(contents, current_chunk, chunk_len, acc)
  |> Ok
}

pub fn join(strings: Array(String), with separator: String) -> String {
  case iv.length(strings) {
    0 | 1 -> strings |> iv.fold("", string.append)
    _ -> {
      let first = strings |> iv.first |> result.unwrap("ERROR")
      let rest = strings |> iv.rest
      intersperse_loop(rest, separator, iv.wrap(first))
      |> iv.fold("", string.append)
    }
  }
}

fn intersperse_loop(
  contents: Result(Array(String), Nil),
  separator: String,
  acc: Array(String),
) -> Array(String) {
  // If it's an error, we're done looping
  let contents_empty = contents |> result.unwrap(iv.new()) |> iv.is_empty
  case contents_empty {
    True -> iv.reverse(acc)
    _ -> {
      // We can safely unwrap because we're done looping
      let contents = contents |> result.unwrap(iv.new())
      let first = contents |> iv.first |> result.unwrap("ERROR")
      let rest = contents |> iv.rest
      let acc = iv.from_list([first, separator]) |> iv.concat(acc)
      intersperse_loop(rest, separator, acc)
    }
  }
}

/// Recursive functio
fn sized_chunk_loop(
  // The contents that haven't been processed yet. If an error, we're done looping
  contents: Result(Array(String), Nil),
  // The current chunk that's being filled
  current_chunk: Array(String),
  chunk_len: Int,
  // The chunks that have already been filled
  acc: Array(Array(String)),
) -> Array(Array(String)) {
  // Finish recursively calling and return the accumulator
  // if the contents are now empty
  use <- bool.guard(result.is_error(contents), acc)

  // We've already guarded against an error above, so we're safe to unwrap
  // Still, just in case, we include the string `ERROR` if somehow we got
  // an error anyways
  let contents = contents |> result.unwrap(iv.wrap("ERROR"))
  let elem = iv.first(contents) |> result.unwrap("ERROR")
  let contents = contents |> iv.rest

  // Whether the current chunk is full
  case iv.length(current_chunk) >= chunk_len {
    False -> {
      let current_chunk = current_chunk |> iv.append(elem)
      sized_chunk_loop(contents, current_chunk, chunk_len, acc)
    }
    True -> {
      // Append the current chunk to the accumulator now that it's done
      let acc = acc |> iv.append(current_chunk)

      // Start the next chunk with the first element that doesn't fit in the previous chunk
      let current_chunk = iv.wrap(elem)

      sized_chunk_loop(contents, current_chunk, chunk_len, acc)
    }
  }
}
