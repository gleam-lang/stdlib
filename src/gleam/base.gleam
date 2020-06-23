import gleam/bit_string.{BitString}
import gleam/string

external fn erl_encode64(BitString) -> String =
  "base64" "encode"

external fn erl_decode64(String) -> Result(BitString, Nil) =
  "gleam_stdlib" "base_decoded4"

pub fn encode64(input: BitString, padding: Bool) -> String {
  let encoded = erl_encode64(input)
  case padding {
    True -> encoded
    False -> string.replace(encoded, "=", "")
  }
}

pub fn decode64(encoded: String) -> Result(BitString, Nil) {
  let padded = case bit_string.byte_size(bit_string.from_string(encoded)) % 4 {
    0 -> encoded
    n -> string.append(encoded, string.repeat("=", 4 - n))
  }
  erl_decode64(padded)
}

pub fn url_encode64(input: BitString, padding: Bool) -> String {
  encode64(input, padding)
  |> string.replace("+", "-")
  |> string.replace("/", "_")
}

pub fn url_decode64(encoded: String) -> Result(BitString, Nil) {
  encoded
  |> string.replace("-", "+")
  |> string.replace("_", "/")
  |> decode64()
}
