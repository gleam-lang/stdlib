import gleam/uri
import gleam/should
import gleam/option.{Option, Some, None}

pub fn full_parse_test() {
  let Ok(
    parsed,
  ) = uri.parse("https://foo:bar@example.com:1234/path?query=true#fragment")
  should.equal(parsed.scheme, Some("https"))
  should.equal(parsed.userinfo, Some("foo:bar"))
  should.equal(parsed.host, Some("example.com"))
  should.equal(parsed.port, Some(1234))
  should.equal(parsed.path, "/path")
  should.equal(parsed.query, Some("query=true"))
  should.equal(parsed.fragment, Some("fragment"))
}

pub fn parse_only_path_test() {
  let Ok(parsed) = uri.parse("")
  should.equal(parsed.scheme, None)
  should.equal(parsed.userinfo, None)
  should.equal(parsed.host, None)
  should.equal(parsed.port, None)
  should.equal(parsed.path, "")
  should.equal(parsed.query, None)
  should.equal(parsed.fragment, None)
}

pub fn parse_only_host_test() {
  let Ok(parsed) = uri.parse("//")
  should.equal(parsed.scheme, None)
  should.equal(parsed.userinfo, None)
  should.equal(parsed.host, Some(""))
  should.equal(parsed.port, None)
  should.equal(parsed.path, "")
  should.equal(parsed.query, None)
  should.equal(parsed.fragment, None)
}

pub fn error_parsing_uri_test() {
  should.equal(uri.parse("::"), Error(Nil))
}

pub fn full_uri_to_string_test() {
  let test_uri = uri.Uri(
    Some("https"),
    Some("foo:bar"),
    Some("example.com"),
    Some(1234),
    "/path",
    Some("query=true"),
    Some("fragment"),
  )
  should.equal(
    uri.to_string(test_uri),
    "https://foo:bar@example.com:1234/path?query=true#fragment",
  )
}

pub fn path_only_uri_to_string_test() {
  let test_uri = uri.Uri(None, None, None, None, "/", None, None)
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

pub fn origin_test() {
  let Ok(parsed) = uri.parse("http://example.test/path?foo#bar")
  uri.origin(parsed)
  |> should.equal(Ok("http://example.test"))

  let Ok(parsed) = uri.parse("http://example.test:8080")
  uri.origin(parsed)
  |> should.equal(Ok("http://example.test:8080"))

  let Ok(parsed) = uri.parse("https://example.test")
  uri.origin(parsed)
  |> should.equal(Ok("https://example.test"))

  let Ok(parsed) = uri.parse("http:///path")
  uri.origin(parsed)
  |> should.equal(Ok("http://"))

  let Ok(parsed) = uri.parse("http://")
  uri.origin(parsed)
  |> should.equal(Ok("http://"))

  let Ok(parsed) = uri.parse("/path")
  uri.origin(parsed)
  |> should.equal(Error(Nil))

  let Ok(parsed) = uri.parse("file:///dev/null")
  uri.origin(parsed)
  |> should.equal(Error(Nil))
}

pub fn merge_test() {
  let Ok(a) = uri.parse("/relative")
  let Ok(b) = uri.parse("")
  uri.merge(a, b)
  |> should.equal(Error(Nil))

  let Ok(a) = uri.parse("http://google.com/foo")
  let Ok(b) = uri.parse("http://example.com/baz")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/baz"))

  let Ok(a) = uri.parse("http://google.com/foo")
  let Ok(b) = uri.parse("http://example.com/.././bar/../../baz")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/baz"))

  let Ok(a) = uri.parse("http://google.com/foo")
  let Ok(b) = uri.parse("//example.com/baz")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/baz"))

  let Ok(a) = uri.parse("http://google.com/foo")
  let Ok(b) = uri.parse("//example.com/.././bar/../../../baz")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/baz"))

  let Ok(a) = uri.parse("http://example.com/foo/bar")
  let Ok(b) = uri.parse("/baz")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/baz"))

  let Ok(a) = uri.parse("http://example.com/foo/bar")
  let Ok(b) = uri.parse("baz")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/foo/baz"))

  let Ok(a) = uri.parse("http://example.com/foo/")
  let Ok(b) = uri.parse("baz")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/foo/baz"))

  let Ok(a) = uri.parse("http://example.com")
  let Ok(b) = uri.parse("baz")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/baz"))

  let Ok(a) = uri.parse("http://example.com")
  let Ok(b) = uri.parse("/.././bar/../../../baz")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/baz"))

  let Ok(a) = uri.parse("http://example.com/foo/bar")
  let Ok(b) = uri.parse("")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/foo/bar"))

  let Ok(a) = uri.parse("http://example.com/foo/bar")
  let Ok(b) = uri.parse("#fragment")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/foo/bar#fragment"))

  let Ok(a) = uri.parse("http://example.com/foo/bar")
  let Ok(b) = uri.parse("?query")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/foo/bar?query"))

  let Ok(a) = uri.parse("http://example.com/foo/bar?query1")
  let Ok(b) = uri.parse("?query2")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/foo/bar?query2"))

  let Ok(a) = uri.parse("http://example.com/foo/bar?query")
  let Ok(b) = uri.parse("")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/foo/bar?query"))
}
