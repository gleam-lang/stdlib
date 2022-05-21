import gleam/should
import gleam/runtime
import gleam/int
import gleam/regex
import gleam/string
import gleam/io

pub fn unix_timestamp_test() {
  let unix_timestamp = runtime.unix_timestamp()

  // Implementation must return an integer
  assert Ok(re) = regex.from_string("[0-9]+")
  unix_timestamp
  |> int.to_string
  |> regex.check(re, _)
  |> should.be_true

  // Implementation must return a positive integer
  unix_timestamp
  |> fn(x) { x >= 0 }
  |> should.be_true
}

pub fn get_locale_test() {
  let locale = runtime.get_locale()

  string.inspect(locale)
  |> should.equal("no-match")

  // Implementation must return a lower case language code
  let language = locale.0
  assert Ok(re) = regex.from_string("[a-z]{2,2}")
  language
  |> regex.check(re, _)
  |> should.be_true

  // Implementation must return an upper case country code
  let country = locale.1
  assert Ok(re) = regex.from_string("[A-Z]{2,2}")
  country
  |> regex.check(re, _)
  |> should.be_true
}
