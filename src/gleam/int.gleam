import gleam/order.{Order}
import gleam/result.{Option}

pub external fn parse(String) -> Option(Int) = "gleam_stdlib" "parse_int";

pub external fn to_string(Int) -> String = "erlang" "integer_to_binary"

pub external fn to_base_string(Int, Int) -> String = "erlang" "integer_to_binary"

pub fn compare(a: Int, b: Int) -> Order {
  case a == b {
    True -> order.Eq
    False ->
      case a < b {
        True -> order.Lt
        False -> order.Gt
      }
  }
}

pub fn min(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}

pub fn max(a: Int, b: Int) -> Int {
  case a > b {
    True -> a
    False -> b
  }
}

