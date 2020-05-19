//// Utilities for working with URIs
////
//// This module provides functions for working with URIs (for example, parsing
//// URIs or encoding query strings). The functions in this module are implemented
//// according to [RFC 3986](https://tools.ietf.org/html/rfc3986).
////
//// Query encoding (Form encoding) is defined in the w3c specification.
//// https://www.w3.org/TR/html52/sec-forms.html#urlencoded-form-data

import gleam/list
import gleam/result.{Option}
import gleam/string
import gleam/dynamic.{Dynamic}

/// Type representing holding the parsed components of an URI.
/// All components of a URI are optional, except the path.
///
pub type Uri {
  Uri(
    scheme: Option(String),
    userinfo: Option(String),
    host: Option(String),
    port: Option(Int),
    path: String,
    query: Option(String),
    fragment: Option(String),
  )
}

/// Parses a complient URI string into the `Uri` Type.
/// If the string is not a valid URI string then an error is returned.
///
/// The opposite operation is `uri.to_string`
///
pub external fn parse(String) -> Result(Uri, Nil) =
  "gleam_uri_native" "parse"

external fn erl_parse_query(String) -> Dynamic =
  "uri_string" "dissect_query"

/// Parses an urlencoded query string into a list of key value pairs.
/// Returns an error for invalid encoding.
///
/// The opposite operation is `uri.query_to_string`.
///
pub fn parse_query(query: String) -> Option(List(tuple(String, String))) {
  query
  |> erl_parse_query
  |> dynamic.list(dynamic.tuple2_of(_, dynamic.string, dynamic.string))
  |> result.map_error(fn(_) { Nil })
}

type Encoding {
  Utf8
}

type ErlQueryToStringOption {
  Encoding(Encoding)
}

external fn erl_query_to_string(
  List(tuple(String, String)),
  List(ErlQueryToStringOption),
) -> Dynamic =
  "uri_string" "compose_query"

/// Encode a list of key value pairs as a URI query string.
///
/// The opposite operation is `uri.parse_query`.
///
pub fn query_to_string(query: List(tuple(String, String))) -> String {
  query
  |> erl_query_to_string([Encoding(Utf8)])
  |> dynamic.string
  |> result.unwrap("")
}

fn do_path_segments(input, accumulator) {
  case input {
    [] -> list.reverse(accumulator)
    [segment, ..rest] -> {
      let accumulator = case segment, accumulator {
        "", accumulator -> accumulator
        ".", accumulator -> accumulator
        "..", [] -> []
        "..", [_, ..accumulator] -> accumulator
        segment, accumulator -> [segment, ..accumulator]
      }
      do_path_segments(rest, accumulator)
    }
  }
}

/// Split the path section of a URI into it's constituent segments.
///
/// Removes empty segments and resolves dot-segments as specified in
/// [section 5.2](https://www.ietf.org/rfc/rfc3986.html#section-5.2) of the RFC.
///
pub fn path_segments(path) {
  do_path_segments(string.split(path, "/"), [])
}

/// Encode a `Uri` value as a URI string.
///
/// The opposite operation is `uri.parse`.
///
pub external fn to_string(Uri) -> String =
  "gleam_uri_native" "to_string"
