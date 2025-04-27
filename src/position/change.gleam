import gleam/order.{type Order}
import gleam/result
import position/offset.{type Offset}
import position/position.{type Position}

pub type Change {
  Change(from: Position, to: Position)
}

/// Simple constructor that takes strings and passes them to the position
/// constructor, so you don't have to manually create positions every single time.
pub fn new(from from_str: String, to to_str: String) -> Result(Change, String) {
  use from <- result.try(from_str |> position.new)
  use to <- result.try(to_str |> position.new)

  Change(from, to) |> Ok
}

pub fn compare(first: Change, second: Change) -> Order {
  // First try to sort on `from` - if you get `Eq`, then sort on `to`. Only if that
  // ALSO returns Eq do we return Eq.
  use <- order.lazy_break_tie(position.compare(first.from, second.from))
  position.compare(first.to, second.to)
}

/// Get the offset of the change - aka the distance the change takes it horizontally
/// and vertically.
pub fn to_offset(change: Change) -> Offset {
  let #(old_rank, old_file) = change.from |> position.to_indices
  let #(new_rank, new_file) = change.to |> position.to_indices

  let vertical_change = new_rank - old_rank
  let horizontal_change = new_file - old_file
  offset.Offset(vertical_change, horizontal_change)
}

pub fn to_string(change: Change) -> String {
  let from_str = change.from |> position.to_string
  let to_str = change.to |> position.to_string

  from_str <> " -> " <> to_str
}
