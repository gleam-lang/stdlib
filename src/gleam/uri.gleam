//// Utilities for working with URIs
////
//// This module provides functions for working with URIs (for example, parsing
//// URIs or encoding query strings). The functions in this module are implemented
//// according to [RFC 3986](https://tools.ietf.org/html/rfc3986).
////
//// Query encoding (Form encoding) is defined in the w3c specification.
//// https://www.w3.org/TR/html52/sec-forms.html#urlencoded-form-data

import gleam/list
import gleam/result
import gleam/option.{None, Option, Some}
import gleam/string
import gleam/dynamic.{Dynamic}
import gleam/map.{Map}
import gleam/function
import gleam/pair

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

pub external fn erl_parse(String) -> Dynamic =
  "uri_string" "parse"

type UriKey {
  Scheme
  Userinfo
  Host
  Port
  Path
  Query
  Fragment
}

/// Parses a compliant URI string into the `Uri` Type.
/// If the string is not a valid URI string then an error is returned.
///
/// The opposite operation is `uri.to_string`
///
pub fn parse(string: String) -> Result(Uri, Nil) {
  try uri_map =
    dynamic.map(erl_parse(string))
    |> result.nil_error
  let get = fn(k: UriKey, decode_type: dynamic.Decoder(t)) -> Option(t) {
    uri_map
    |> map.get(dynamic.from(k))
    |> result.then(function.compose(decode_type, result.nil_error))
    |> option.from_result
  }

  let uri =
    Uri(
      scheme: get(Scheme, dynamic.string),
      userinfo: get(Userinfo, dynamic.string),
      host: get(Host, dynamic.string),
      port: get(Port, dynamic.int),
      path: option.unwrap(get(Path, dynamic.string), ""),
      query: get(Query, dynamic.string),
      fragment: get(Fragment, dynamic.string),
    )
  Ok(uri)
}

external fn erl_parse_query(String) -> Dynamic =
  "uri_string" "dissect_query"

/// Parses an urlencoded query string into a list of key value pairs.
/// Returns an error for invalid encoding.
///
/// The opposite operation is `uri.query_to_string`.
///
pub fn parse_query(query: String) -> Result(List(tuple(String, String)), Nil) {
  let bool_value = fn(x) { result.map(dynamic.bool(x), fn(_) { "" }) }
  let query_param = dynamic.typed_tuple2(
    _,
    dynamic.string,
    dynamic.any(_, of: [dynamic.string, bool_value]),
  )

  query
  |> erl_parse_query
  |> dynamic.typed_list(of: query_param)
  |> result.nil_error
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

/// Encode a string into a percent encoded representation.
/// Note that this encodes space as +.
///
/// ## Example
///
/// percent_encode("100% great")
/// > "100%25+great"
///
pub fn percent_encode(value: String) -> String {
  query_to_string([tuple("k", value)])
  |> string.replace(each: "k=", with: "")
}

/// Decode a percent encoded string.
///
/// ## Example
///
/// percent_decode("100%25+great")
/// > Ok("100% great")
///
pub fn percent_decode(value: String) -> Result(String, Nil) {
  string.concat(["k=", value])
  |> parse_query
  |> result.then(list.head)
  |> result.map(pair.second)
}

fn do_remove_dot_segments(
  input: List(String),
  accumulator: List(String),
) -> List(String) {
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
      do_remove_dot_segments(rest, accumulator)
    }
  }
}

fn remove_dot_segments(input: List(String)) -> List(String) {
  do_remove_dot_segments(input, [])
}

/// Split the path section of a URI into it's constituent segments.
///
/// Removes empty segments and resolves dot-segments as specified in
/// [section 5.2](https://www.ietf.org/rfc/rfc3986.html#section-5.2) of the RFC.
///
pub fn path_segments(path: String) -> List(String) {
  remove_dot_segments(string.split(path, "/"))
}

external fn erl_to_string(Map(UriKey, Dynamic)) -> Dynamic =
  "uri_string" "recompose"

/// Encode a `Uri` value as a URI string.
///
/// The opposite operation is `uri.parse`.
///
pub fn to_string(uri: Uri) -> String {
  let field = fn(key: UriKey, value: Option(anything)) -> Result(
    tuple(UriKey, Dynamic),
    Nil,
  ) {
    case value {
      Some(v) -> Ok(tuple(key, dynamic.from(v)))
      None -> Error(Nil)
    }
  }

  [
    field(Scheme, uri.scheme),
    field(Userinfo, uri.userinfo),
    field(Host, uri.host),
    field(Port, uri.port),
    field(Path, option.Some(uri.path)),
    field(Query, uri.query),
    field(Fragment, uri.fragment),
  ]
  |> list.filter_map(fn(x) { x })
  |> map.from_list
  |> erl_to_string
  |> dynamic.string
  |> result.unwrap("")
}

/// Fetch the origin of a uri
///
/// Return the origin of a uri as defined in
/// https://tools.ietf.org/html/rfc6454
///
/// The supported uri schemes are `http` and `https`
/// Urls without a scheme will return Error
pub fn origin(uri: Uri) -> Result(String, Nil) {
  let Uri(scheme: scheme, host: host, port: port, ..) = uri
  case scheme {
    Some("https") | Some("http") -> {
      let origin = Uri(scheme, None, host, port, "", None, None)
      Ok(to_string(origin))
    }
    _ -> Error(Nil)
  }
}

fn drop_last(elements: List(a)) -> List(a) {
  list.take(from: elements, up_to: list.length(elements) - 1)
}

fn join_segments(segments: List(String)) -> String {
  string.join(["", ..segments], "/")
}

/// Resolve a uri with respect to the given base uri
///
/// The base uri must be an absolute uri or this function will return an error.
/// The algorithm for merging uris is described in [RFC 3986](https://tools.ietf.org/html/rfc3986#section-5.2)
pub fn merge(base: Uri, relative: Uri) -> Result(Uri, Nil) {
  case base {
    Uri(scheme: Some(_), host: Some(_), ..) ->
      case relative {
        Uri(host: Some(_), ..) -> {
          let path =
            string.split(relative.path, "/")
            |> remove_dot_segments()
            |> join_segments()
          let resolved =
            Uri(
              option.or(relative.scheme, base.scheme),
              None,
              relative.host,
              relative.port,
              path,
              relative.query,
              relative.fragment,
            )
          Ok(resolved)
        }
        Uri(scheme: None, host: None, ..) -> {
          let tuple(new_path, new_query) = case relative.path {
            "" -> tuple(base.path, option.or(relative.query, base.query))
            _ -> {
              let path_segments = case string.starts_with(relative.path, "/") {
                True -> string.split(relative.path, "/")
                False ->
                  string.split(base.path, "/")
                  |> drop_last()
                  |> list.append(string.split(relative.path, "/"))
              }
              let path =
                path_segments
                |> remove_dot_segments()
                |> join_segments()
              tuple(path, relative.query)
            }
          }
          let resolved =
            Uri(
              base.scheme,
              None,
              base.host,
              base.port,
              new_path,
              new_query,
              relative.fragment,
            )
          Ok(resolved)
        }
      }
    _ -> Error(Nil)
  }
}
