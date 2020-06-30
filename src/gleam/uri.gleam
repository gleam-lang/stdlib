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

/// Parses a complient URI string into the `Uri` Type.
/// If the string is not a valid URI string then an error is returned.
///
/// The opposite operation is `uri.to_string`
///
pub fn parse(string: String) -> Result(Uri, Nil) {
  try uri_map = dynamic.map(erl_parse(string))
    |> result.nil_error
  let get = fn(k: UriKey, decode_type: dynamic.Decoder(t)) -> Option(t) {
    uri_map
    |> map.get(dynamic.from(k))
    |> result.then(function.compose(decode_type, result.nil_error))
    |> option.from_result
  }

  let uri = Uri(
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
  query
  |> erl_parse_query
  |> dynamic.typed_list(dynamic.typed_tuple2(_, dynamic.string, dynamic.string))
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

fn do_remove_dot_segments(input, accumulator) {
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

fn remove_dot_segments(input) {
  do_remove_dot_segments(input, [])
}

/// Split the path section of a URI into it's constituent segments.
///
/// Removes empty segments and resolves dot-segments as specified in
/// [section 5.2](https://www.ietf.org/rfc/rfc3986.html#section-5.2) of the RFC.
///
pub fn path_segments(path) {
  remove_dot_segments(string.split(path, "/"))
}

external fn erl_to_string(Map(UriKey, Dynamic)) -> Dynamic =
  "uri_string" "recompose"

/// Encode a `Uri` value as a URI string.
///
/// The opposite operation is `uri.parse`.
///
pub fn to_string(uri: Uri) -> String {
  let field = fn(key: UriKey, value: Option(anything)) {
    value
    |> option.to_result(Nil)
    |> result.map(fn(value) { tuple(key, dynamic.from(value)) })
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

fn drop_last(elements) {
  let tuple(keep, _last) = list.split(elements, list.length(elements) - 1)
  keep
}

fn join_segments(segments) {
  case segments {
    [] -> ""
    _ -> string.append("/", string.join(segments, "/"))
  }
}

/// Resolve a uri with respect to the given base uri
///
/// The base uri must be an absolute uri or this function will return an error.
/// The algorithm for merging uris is described in [RFC 3986](https://tools.ietf.org/html/rfc3986#section-5.2)
pub fn merge(base: Uri, relative: Uri) -> Result(Uri, Nil) {
  case base {
    Uri(scheme: Some(_), host: Some(_), ..) -> case relative {
      Uri(scheme: Some(_), ..) -> {
        let path = string.split(relative.path, "/")
          |> remove_dot_segments()
          |> join_segments()
        let resolved = Uri(
          relative.scheme,
          None,
          relative.host,
          relative.port,
          path,
          relative.query,
          relative.fragment,
        )
        Ok(resolved)
      }
      Uri(scheme: None, host: Some(_), ..) -> {
        let path = string.split(relative.path, "/")
          |> remove_dot_segments()
          |> join_segments()
        let resolved = Uri(
          base.scheme,
          None,
          relative.host,
          relative.port,
          path,
          relative.query,
          relative.fragment,
        )
        Ok(resolved)
      }
      Uri(scheme: None, host: None, path: "", ..) -> {
        let resolved = Uri(
          base.scheme,
          None,
          base.host,
          base.port,
          base.path,
          option.or(relative.query, base.query),
          relative.fragment,
        )
        Ok(resolved)
      }
      Uri(
        scheme: None,
        host: None,
        path: path,
        ..,
      ) -> case string.starts_with(path, "/") {
        True -> {
          let path = string.split(relative.path, "/")
            |> remove_dot_segments()
            |> join_segments()
          let resolved = Uri(
            base.scheme,
            None,
            base.host,
            base.port,
            path,
            relative.query,
            relative.fragment,
          )
          Ok(resolved)
        }
        False -> {
          let path = string.split(base.path, "/")
            |> drop_last()
            |> list.append(string.split(relative.path, "/"))
            |> remove_dot_segments()
            |> join_segments()
          let resolved = Uri(
            base.scheme,
            None,
            base.host,
            base.port,
            path,
            relative.query,
            relative.fragment,
          )
          Ok(resolved)
        }
      }
    }
    _ -> Error(Nil)
  }
}
