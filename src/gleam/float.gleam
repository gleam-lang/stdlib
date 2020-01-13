import gleam/iodata
import gleam/order.{Order}

pub external fn parse(String) -> Result(Float, Nil)
  = "gleam_stdlib" "parse_float";

pub fn to_string(f: Float) -> String {
  f
  |> iodata.from_float
  |> iodata.to_string
}

pub fn compare(a: Float, b: Float) -> Order {
  case a == b {
    True -> order.Eq
    False ->
    case a <. b {
      True -> order.Lt
      False -> order.Gt
    }
  }
}

pub fn min(a: Float, b: Float) -> Float {
  case a <. b {
    True -> a
    False -> b
  }
}

pub fn max(a: Float, b: Float) -> Float {
  case a >. b {
    True -> a
    False -> b
  }
}

pub external fn ceiling(Float) -> Float = "math" "ceil";

pub external fn floor(Float) -> Float = "math" "floor";

pub external fn round(Float) -> Int = "erlang" "round";

pub external fn truncate(Float) -> Int = "erlang" "trunc";
