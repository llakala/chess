import gleam/list
import gleam/order.{Eq, Gt, Lt}

pub fn min(
  over list: List(a),
  with compare: fn(a, a) -> order.Order,
) -> Result(a, Nil) {
  list.reduce(over: list, with: fn(acc, other) {
    case compare(acc, other) {
      Lt -> acc
      Gt | Eq -> other
    }
  })
}
