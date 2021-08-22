import gleam/uri
import gleam/should
import gleam/option.{None, Some}

if erlang {
  import gleam/string
  import gleam/list

  pub fn full_parse_test() {
    assert Ok(parsed) =
      uri.parse("https://foo:bar@example.com:1234/path?query=true#fragment")
    should.equal(parsed.scheme, Some("https"))
    should.equal(parsed.userinfo, Some("foo:bar"))
    should.equal(parsed.host, Some("example.com"))
    should.equal(parsed.port, Some(1234))
    should.equal(parsed.path, "/path")
    should.equal(parsed.query, Some("query=true"))
    should.equal(parsed.fragment, Some("fragment"))
  }

  pub fn parse_only_path_test() {
    assert Ok(parsed) = uri.parse("")
    should.equal(parsed.scheme, None)
    should.equal(parsed.userinfo, None)
    should.equal(parsed.host, None)
    should.equal(parsed.port, None)
    should.equal(parsed.path, "")
    should.equal(parsed.query, None)
    should.equal(parsed.fragment, None)
  }

  pub fn parse_only_host_test() {
    assert Ok(parsed) = uri.parse("//")
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
    let test_uri =
      uri.Uri(
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
}

pub fn path_only_uri_to_string_test() {
  uri.Uri(None, None, None, None, "/", None, None)
  |> uri.to_string
  |> should.equal("/")

  uri.Uri(None, None, None, None, "/teapot", None, None)
  |> uri.to_string
  |> should.equal("/teapot")

  uri.Uri(None, Some("user"), None, None, "/teapot", None, None)
  |> uri.to_string
  |> should.equal("/teapot")

  uri.Uri(None, None, None, None, "", None, None)
  |> uri.to_string
  |> should.equal("")
}

pub fn scheme_to_string_test() {
  uri.Uri(Some("ftp"), None, None, None, "thing.txt", None, None)
  |> uri.to_string
  |> should.equal("ftp:thing.txt")

  uri.Uri(Some("ftp"), None, None, None, "", None, None)
  |> uri.to_string
  |> should.equal("ftp:")

  uri.Uri(Some("ftp"), Some("ignored"), None, None, "", None, None)
  |> uri.to_string
  |> should.equal("ftp:")

  uri.Uri(Some("https"), None, None, None, "/one/two", None, None)
  |> uri.to_string
  |> should.equal("https:/one/two")

  uri.Uri(None, None, None, None, "noslash", None, Some("frag"))
  |> uri.to_string
  |> should.equal("noslash#frag")
}

pub fn host_to_string_test() {
  uri.Uri(Some("ftp"), None, Some("example.com"), None, "", None, None)
  |> uri.to_string
  |> should.equal("ftp://example.com/")

  uri.Uri(None, None, Some("example.com"), None, "", None, None)
  |> uri.to_string
  |> should.equal("//example.com/")

  uri.Uri(None, None, Some("example.com"), None, "/slash", None, None)
  |> uri.to_string
  |> should.equal("//example.com/slash")

  uri.Uri(None, None, Some("example.com"), None, "noslash", None, None)
  |> uri.to_string
  |> should.equal("//example.com/noslash")

  uri.Uri(None, None, Some(""), None, "", None, None)
  |> uri.to_string
  |> should.equal("//")

  uri.Uri(None, None, Some("example.com"), None, "noslash", None, Some("ok"))
  |> uri.to_string
  |> should.equal("//example.com/noslash#ok")

  uri.Uri(None, None, Some(""), None, "", None, Some("ok"))
  |> uri.to_string
  |> should.equal("//#ok")
}

pub fn port_to_string_test() {
  uri.Uri(Some("ftp"), None, Some("example.com"), Some(80), "", None, None)
  |> uri.to_string
  |> should.equal("ftp://example.com:80/")

  uri.Uri(None, None, Some("example.com"), Some(40), "", None, None)
  |> uri.to_string
  |> should.equal("//example.com:40/")

  uri.Uri(None, None, Some("example.com"), Some(80), "/slash", None, None)
  |> uri.to_string
  |> should.equal("//example.com:80/slash")

  uri.Uri(None, None, Some("example.com"), Some(81), "noslash", None, None)
  |> uri.to_string
  |> should.equal("//example.com:81/noslash")

  uri.Uri(None, None, None, Some(81), "noslash", None, None)
  |> uri.to_string
  |> should.equal("noslash")
}

if erlang {
  pub fn parse_query_string_test() {
    assert Ok(parsed) = uri.parse_query("foo+bar=1&city=%C3%B6rebro")
    should.equal(parsed, [#("foo bar", "1"), #("city", "örebro")])

    // Duplicates keys not overridden
    assert Ok(parsed) = uri.parse_query("a[]=1&a[]=2")

    parsed
    |> should.equal([#("a[]", "1"), #("a[]", "2")])
  }

  pub fn parse_empty_query_string_test() {
    assert Ok(parsed) = uri.parse_query("")
    should.equal(parsed, [])
  }

  pub fn parse_query_string_with_empty_test() {
    uri.parse_query("present")
    |> should.equal(Ok([#("present", "")]))
  }

  pub fn error_parsing_query_test() {
    should.equal(uri.parse_query("%C2"), Error(Nil))
  }

  pub fn query_to_string_test() {
    let query_string =
      uri.query_to_string([#("foo bar", "1"), #("city", "örebro")])
    should.equal(query_string, "foo+bar=1&city=%C3%B6rebro")
  }

  pub fn empty_query_to_string_test() {
    let query_string = uri.query_to_string([])
    should.equal(query_string, "")
  }

  fn percent_codec_fixtures() {
    [
      #(" ", "+"),
      #(",", "%2C"),
      #(";", "%3B"),
      #(":", "%3A"),
      #("!", "%21"),
      #("?", "%3F"),
      #("'", "%27"),
      #("(", "%28"),
      #(")", "%29"),
      #("[", "%5B"),
      #("@", "%40"),
      #("/", "%2F"),
      #("\\", "%5C"),
      #("&", "%26"),
      #("#", "%23"),
      #("=", "%3D"),
      #("~", "%7E"),
      #("ñ", "%C3%B1"),
      // Allowed chars
      #("-", "-"),
      #("_", "_"),
      #(".", "."),
      #("*", "*"),
      #("100% great", "100%25+great"),
    ]
  }

  pub fn percent_encode_test() {
    percent_codec_fixtures()
    |> list.map(fn(t) {
      let #(a, b) = t
      uri.percent_encode(a)
      |> should.equal(b)
    })
  }

  pub fn percent_encode_consistency_test() {
    let k = "foo bar[]"
    let v = "ñaña (,:*~)"

    let query_string = uri.query_to_string([#(k, v)])

    let encoded_key = uri.percent_encode(k)
    let encoded_value = uri.percent_encode(v)
    let manual_query_string = string.concat([encoded_key, "=", encoded_value])

    should.equal(query_string, manual_query_string)
  }

  pub fn percent_decode_test() {
    percent_codec_fixtures()
    |> list.map(fn(t) {
      let #(a, b) = t
      uri.percent_decode(b)
      |> should.equal(Ok(a))
    })
  }

  pub fn percent_decode_consistency_test() {
    let k = "foo+bar[]"
    let v = "%C3%B6rebro"
    let query = string.concat([k, "=", v])
    assert Ok(parsed) = uri.parse_query(query)

    assert Ok(decoded_key) = uri.percent_decode(k)
    assert Ok(decoded_value) = uri.percent_decode(v)

    should.equal(parsed, [#(decoded_key, decoded_value)])
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
    assert Ok(parsed) = uri.parse("http://example.test/path?foo#bar")
    uri.origin(parsed)
    |> should.equal(Ok("http://example.test/"))

    assert Ok(parsed) = uri.parse("http://example.test:8080")
    uri.origin(parsed)
    |> should.equal(Ok("http://example.test:8080/"))

    assert Ok(parsed) = uri.parse("https://example.test")
    uri.origin(parsed)
    |> should.equal(Ok("https://example.test/"))

    assert Ok(parsed) = uri.parse("http:///path")
    uri.origin(parsed)
    |> should.equal(Ok("http://"))

    assert Ok(parsed) = uri.parse("http://")
    uri.origin(parsed)
    |> should.equal(Ok("http://"))

    assert Ok(parsed) = uri.parse("/path")
    uri.origin(parsed)
    |> should.equal(Error(Nil))

    assert Ok(parsed) = uri.parse("file:///dev/null")
    uri.origin(parsed)
    |> should.equal(Error(Nil))
  }

  pub fn merge_test() {
    assert Ok(a) = uri.parse("/relative")
    assert Ok(b) = uri.parse("")
    uri.merge(a, b)
    |> should.equal(Error(Nil))

    assert Ok(a) = uri.parse("http://google.com/foo")
    assert Ok(b) = uri.parse("http://example.com/baz")
    uri.merge(a, b)
    |> should.equal(uri.parse("http://example.com/baz"))

    assert Ok(a) = uri.parse("http://google.com/foo")
    assert Ok(b) = uri.parse("http://example.com/.././bar/../../baz")
    uri.merge(a, b)
    |> should.equal(uri.parse("http://example.com/baz"))

    assert Ok(a) = uri.parse("http://google.com/foo")
    assert Ok(b) = uri.parse("//example.com/baz")
    uri.merge(a, b)
    |> should.equal(uri.parse("http://example.com/baz"))

    assert Ok(a) = uri.parse("http://google.com/foo")
    assert Ok(b) = uri.parse("//example.com/.././bar/../../../baz")
    uri.merge(a, b)
    |> should.equal(uri.parse("http://example.com/baz"))

    assert Ok(a) = uri.parse("http://example.com/foo/bar")
    assert Ok(b) = uri.parse("/baz")
    uri.merge(a, b)
    |> should.equal(uri.parse("http://example.com/baz"))

    assert Ok(a) = uri.parse("http://example.com/foo/bar")
    assert Ok(b) = uri.parse("baz")
    uri.merge(a, b)
    |> should.equal(uri.parse("http://example.com/foo/baz"))

    assert Ok(a) = uri.parse("http://example.com/foo/")
    assert Ok(b) = uri.parse("baz")
    uri.merge(a, b)
    |> should.equal(uri.parse("http://example.com/foo/baz"))

    assert Ok(a) = uri.parse("http://example.com")
    assert Ok(b) = uri.parse("baz")
    uri.merge(a, b)
    |> should.equal(uri.parse("http://example.com/baz"))

    assert Ok(a) = uri.parse("http://example.com")
    assert Ok(b) = uri.parse("/.././bar/../../../baz")
    uri.merge(a, b)
    |> should.equal(uri.parse("http://example.com/baz"))

    assert Ok(a) = uri.parse("http://example.com/foo/bar")
    assert Ok(b) = uri.parse("")
    uri.merge(a, b)
    |> should.equal(uri.parse("http://example.com/foo/bar"))

    assert Ok(a) = uri.parse("http://example.com/foo/bar")
    assert Ok(b) = uri.parse("#fragment")
    uri.merge(a, b)
    |> should.equal(uri.parse("http://example.com/foo/bar#fragment"))

    assert Ok(a) = uri.parse("http://example.com/foo/bar")
    assert Ok(b) = uri.parse("?query")
    uri.merge(a, b)
    |> should.equal(uri.parse("http://example.com/foo/bar?query"))

    assert Ok(a) = uri.parse("http://example.com/foo/bar?query1")
    assert Ok(b) = uri.parse("?query2")
    uri.merge(a, b)
    |> should.equal(uri.parse("http://example.com/foo/bar?query2"))

    assert Ok(a) = uri.parse("http://example.com/foo/bar?query")
    assert Ok(b) = uri.parse("")
    uri.merge(a, b)
    |> should.equal(uri.parse("http://example.com/foo/bar?query"))
  }
}
