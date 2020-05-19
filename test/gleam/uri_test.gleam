import gleam/uri
import gleam/should

pub fn full_parse_test() {
  let Ok(
    parsed,
  ) = uri.parse("https://foo:bar@example.com:1234/path?query=true#fragment")
  should.equal(parsed.scheme, Ok("https"))
  should.equal(parsed.userinfo, Ok("foo:bar"))
  should.equal(parsed.host, Ok("example.com"))
  should.equal(parsed.port, Ok(1234))
  should.equal(parsed.path, "/path")
  should.equal(parsed.query, Ok("query=true"))
  should.equal(parsed.fragment, Ok("fragment"))
}

pub fn parse_only_path_test() {
  let Ok(parsed) = uri.parse("")
  should.equal(parsed.scheme, Error(Nil))
  should.equal(parsed.userinfo, Error(Nil))
  should.equal(parsed.host, Error(Nil))
  should.equal(parsed.port, Error(Nil))
  should.equal(parsed.path, "")
  should.equal(parsed.query, Error(Nil))
  should.equal(parsed.fragment, Error(Nil))
}

pub fn parse_only_host_test() {
  let Ok(parsed) = uri.parse("//")
  should.equal(parsed.scheme, Error(Nil))
  should.equal(parsed.userinfo, Error(Nil))
  should.equal(parsed.host, Ok(""))
  should.equal(parsed.port, Error(Nil))
  should.equal(parsed.path, "")
  should.equal(parsed.query, Error(Nil))
  should.equal(parsed.fragment, Error(Nil))
}

pub fn error_parsing_uri_test() {
  should.equal(uri.parse("::"), Error(Nil))
}

pub fn full_uri_to_string_test() {
  let test_uri = uri.Uri(
    Ok("https"),
    Ok("foo:bar"),
    Ok("example.com"),
    Ok(1234),
    "/path",
    Ok("query=true"),
    Ok("fragment"),
  )
  should.equal(
    uri.to_string(test_uri),
    "https://foo:bar@example.com:1234/path?query=true#fragment",
  )
}

pub fn path_only_uri_to_string_test() {
  let test_uri = uri.Uri(
    Error(Nil),
    Error(Nil),
    Error(Nil),
    Error(Nil),
    "/",
    Error(Nil),
    Error(Nil),
  )
  should.equal(uri.to_string(test_uri), "/")
}

pub fn parse_query_string_test() {
  let Ok(parsed) = uri.parse_query("foo+bar=1&city=%C3%B6rebro")
  should.equal(parsed, [tuple("foo bar", "1"), tuple("city", "örebro")])
}

pub fn parse_empty_query_string_test() {
  let Ok(parsed) = uri.parse_query("")
  should.equal(parsed, [])
}

pub fn error_parsing_query_test() {
  should.equal(uri.parse_query("%C2"), Error(Nil))
}

pub fn query_to_string_test() {
  let query_string = uri.query_to_string(
    [tuple("foo bar", "1"), tuple("city", "örebro")],
  )
  should.equal(query_string, "foo+bar=1&city=%C3%B6rebro")
}

pub fn empty_query_to_string_test() {
  let query_string = uri.query_to_string([])
  should.equal(query_string, "")
}

pub fn parse_segments_test() {
  should.equal(uri.path_segments("/"), [])
  should.equal(uri.path_segments("/foo/bar"), ["foo", "bar"])
  should.equal(uri.path_segments("////"), [])
  should.equal(uri.path_segments("/foo//bar"), ["foo", "bar"])

  should.equal(uri.path_segments("/."), [])
  should.equal(uri.path_segments("/.foo"), [".foo"])

  should.equal(uri.path_segments("/../bar"), ["bar"])
  should.equal(uri.path_segments("../bar"), ["bar"])
  should.equal(uri.path_segments("/foo/../bar"), ["bar"])
}
