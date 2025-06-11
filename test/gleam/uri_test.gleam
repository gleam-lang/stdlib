// TODO: IPv6 URI parse tests
// https://github.com/elixir-lang/elixir/blob/2d43b9670f54c4d8e0be1ee4d2ee8f99d7378480/lib/elixir/test/elixir/uri_test.exs
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleam/uri

pub fn full_parse_test() {
  let assert Ok(parsed) =
    uri.parse("https://weebl:bob@example.com:1234/path?query=true#fragment")
  assert parsed.scheme == Some("https")
  assert parsed.userinfo == Some("weebl:bob")
  assert parsed.host == Some("example.com")
  assert parsed.port == Some(1234)
  assert parsed.path == "/path"
  assert parsed.query == Some("query=true")
  assert parsed.fragment == Some("fragment")
}

pub fn parse_only_path_test() {
  let assert Ok(parsed) = uri.parse("")
  assert parsed.scheme == None
  assert parsed.userinfo == None
  assert parsed.host == None
  assert parsed.port == None
  assert parsed.path == ""
  assert parsed.query == None
  assert parsed.fragment == None
}

pub fn parse_only_host_test() {
  let assert Ok(parsed) = uri.parse("//")
  assert parsed.scheme == None
  assert parsed.userinfo == None
  assert parsed.host == Some("")
  assert parsed.port == None
  assert parsed.path == ""
  assert parsed.query == None
  assert parsed.fragment == None
}

pub fn parse_scheme_test() {
  assert uri.parse("http://one.com/path/to/something?one=two&two=one#fragment")
    == Ok(uri.Uri(
      scheme: Some("http"),
      host: Some("one.com"),
      path: "/path/to/something",
      query: Some("one=two&two=one"),
      fragment: Some("fragment"),
      port: None,
      userinfo: None,
    ))
}

pub fn parse_https_scheme_test() {
  assert uri.parse("https://foo.com")
    == Ok(uri.Uri(
      scheme: Some("https"),
      host: Some("foo.com"),
      path: "",
      query: None,
      fragment: None,
      port: None,
      userinfo: None,
    ))
}

pub fn parse_file_scheme_test() {
  assert uri.parse("file:///one/two/three")
    == Ok(uri.Uri(
      scheme: Some("file"),
      host: Some(""),
      path: "/one/two/three",
      query: None,
      fragment: None,
      port: None,
      userinfo: None,
    ))
}

pub fn parse_ftp_scheme_test() {
  assert uri.parse(
      "ftp://user001:password@private.ftp-server.example.com/my_directory/my_file.txt",
    )
    == Ok(uri.Uri(
      scheme: Some("ftp"),
      host: Some("private.ftp-server.example.com"),
      userinfo: Some("user001:password"),
      path: "/my_directory/my_file.txt",
      query: None,
      fragment: None,
      port: None,
    ))
}

pub fn parse_sftp_scheme_test() {
  assert uri.parse(
      "sftp://user001:password@private.ftp-server.example.com/my_directory/my_file.txt",
    )
    == Ok(uri.Uri(
      scheme: Some("sftp"),
      host: Some("private.ftp-server.example.com"),
      userinfo: Some("user001:password"),
      path: "/my_directory/my_file.txt",
      query: None,
      fragment: None,
      port: None,
    ))
}

pub fn parse_tftp_scheme_test() {
  assert uri.parse(
      "tftp://user001:password@private.ftp-server.example.com/my_directory/my_file.txt",
    )
    == Ok(uri.Uri(
      scheme: Some("tftp"),
      host: Some("private.ftp-server.example.com"),
      userinfo: Some("user001:password"),
      path: "/my_directory/my_file.txt",
      query: None,
      fragment: None,
      port: None,
    ))
}

pub fn parse_ldap_scheme_test() {
  assert uri.parse("ldap:///dc=example,dc=com??sub?(givenName=John)")
    == Ok(uri.Uri(
      scheme: Some("ldap"),
      host: Some(""),
      userinfo: None,
      path: "/dc=example,dc=com",
      query: Some("?sub?(givenName=John)"),
      fragment: None,
      port: None,
    ))
}

pub fn parse_ldap_2_scheme_test() {
  assert uri.parse("ldap://ldap.example.com/cn=John%20Doe,dc=foo,dc=com")
    == Ok(uri.Uri(
      scheme: Some("ldap"),
      host: Some("ldap.example.com"),
      userinfo: None,
      path: "/cn=John%20Doe,dc=foo,dc=com",
      query: None,
      fragment: None,
      port: None,
    ))
}

fn assert_parse(s) {
  let assert Ok(u) = uri.parse(s)
  u
}

// TODO: Assert these do not parse
// pub fn parse_bad_uris_test() {
//   uri.parse("")
//   uri.parse("https:??@?F?@#>F//23/")
//   assert ":https" = uri.parse(":https").path
//   assert "https" = uri.parse("https").path
// }

pub fn parse_downcases_scheme() {
  let assert Ok(uri) = uri.parse("HTTPS://EXAMPLE.COM")
  let assert Some("https") = uri.scheme
  let assert Some("EXAMPLE.COM") = uri.host
}

pub fn parse_empty_fragments_test() {
  let assert Some("") = assert_parse("http://example.com#").fragment
  let assert Some("") = assert_parse("http://example.com/#").fragment
  let assert Some("") = assert_parse("http://example.com/test#").fragment
}

pub fn parse_empty_queries_test() {
  let assert Some("") = assert_parse("http://example.com?").query
  let assert Some("") = assert_parse("http://example.com/?").query
  let assert Some("") = assert_parse("http://example.com/test?").query
}

pub fn full_uri_to_string_test() {
  let test_uri =
    uri.Uri(
      Some("https"),
      Some("weebl:bob"),
      Some("example.com"),
      Some(1234),
      "/path",
      Some("query=true"),
      Some("fragment"),
    )
  assert uri.to_string(test_uri)
    == "https://weebl:bob@example.com:1234/path?query=true#fragment"
}

pub fn path_only_uri_to_string_test() {
  assert uri.to_string(uri.Uri(None, None, None, None, "/", None, None)) == "/"

  assert uri.to_string(uri.Uri(None, None, None, None, "/teapot", None, None))
    == "/teapot"

  assert uri.to_string(uri.Uri(
      None,
      Some("user"),
      None,
      None,
      "/teapot",
      None,
      None,
    ))
    == "/teapot"

  assert uri.to_string(uri.Uri(None, None, None, None, "", None, None)) == ""
}

pub fn scheme_to_string_test() {
  assert uri.to_string(uri.Uri(
      Some("ftp"),
      None,
      None,
      None,
      "thing.txt",
      None,
      None,
    ))
    == "ftp:thing.txt"

  assert uri.to_string(uri.Uri(Some("ftp"), None, None, None, "", None, None))
    == "ftp:"

  assert uri.to_string(uri.Uri(
      Some("ftp"),
      Some("ignored"),
      None,
      None,
      "",
      None,
      None,
    ))
    == "ftp:"

  assert uri.to_string(uri.Uri(
      Some("https"),
      None,
      None,
      None,
      "/one/two",
      None,
      None,
    ))
    == "https:/one/two"

  assert uri.to_string(uri.Uri(
      None,
      None,
      None,
      None,
      "noslash",
      None,
      Some("frag"),
    ))
    == "noslash#frag"
}

pub fn host_to_string_test() {
  assert uri.to_string(uri.Uri(
      Some("ftp"),
      None,
      Some("example.com"),
      None,
      "",
      None,
      None,
    ))
    == "ftp://example.com/"

  assert uri.to_string(uri.Uri(
      None,
      None,
      Some("example.com"),
      None,
      "",
      None,
      None,
    ))
    == "//example.com/"

  assert uri.to_string(uri.Uri(
      None,
      None,
      Some("example.com"),
      None,
      "/slash",
      None,
      None,
    ))
    == "//example.com/slash"

  assert uri.to_string(uri.Uri(
      None,
      None,
      Some("example.com"),
      None,
      "noslash",
      None,
      None,
    ))
    == "//example.com/noslash"

  assert uri.to_string(uri.Uri(None, None, Some(""), None, "", None, None))
    == "//"

  assert uri.to_string(uri.Uri(
      None,
      None,
      Some("example.com"),
      None,
      "noslash",
      None,
      Some("ok"),
    ))
    == "//example.com/noslash#ok"

  assert uri.to_string(uri.Uri(None, None, Some(""), None, "", None, Some("ok")))
    == "//#ok"
}

pub fn port_to_string_test() {
  assert uri.to_string(uri.Uri(
      Some("ftp"),
      None,
      Some("example.com"),
      Some(80),
      "",
      None,
      None,
    ))
    == "ftp://example.com:80/"

  assert uri.to_string(uri.Uri(
      None,
      None,
      Some("example.com"),
      Some(40),
      "",
      None,
      None,
    ))
    == "//example.com:40/"

  assert uri.to_string(uri.Uri(
      None,
      None,
      Some("example.com"),
      Some(80),
      "/slash",
      None,
      None,
    ))
    == "//example.com:80/slash"

  assert uri.to_string(uri.Uri(
      None,
      None,
      Some("example.com"),
      Some(81),
      "noslash",
      None,
      None,
    ))
    == "//example.com:81/noslash"

  assert uri.to_string(uri.Uri(
      None,
      None,
      None,
      Some(81),
      "noslash",
      None,
      None,
    ))
    == "noslash"
}

pub fn parse_query_string_test() {
  let assert Ok(parsed) = uri.parse_query("weebl+bob=1&city=%C3%B6rebro")
  assert parsed == [#("weebl bob", "1"), #("city", "örebro")]

  // Duplicates keys not overridden
  let assert Ok(parsed) = uri.parse_query("a[]=1&a[]=2")

  assert parsed == [#("a[]", "1"), #("a[]", "2")]
}

pub fn parse_empty_query_string_test() {
  let assert Ok(parsed) = uri.parse_query("")
  assert parsed == []
}

pub fn parse_query_string_with_empty_test() {
  assert uri.parse_query("present") == Ok([#("present", "")])
}

pub fn error_parsing_query_test() {
  assert uri.parse_query("%C2") == Error(Nil)
}

pub fn query_to_string_test() {
  let query_string =
    uri.query_to_string([#("weebl bob", "1"), #("city", "örebro")])
  assert query_string == "weebl%20bob=1&city=%C3%B6rebro"
}

pub fn empty_query_to_string_test() {
  let query_string = uri.query_to_string([])
  assert query_string == ""
}

const percent_codec_fixtures = [
  #(" ", "%20"),
  #(",", "%2C"),
  #(";", "%3B"),
  #(":", "%3A"),
  #("!", "!"),
  #("?", "%3F"),
  #("'", "'"),
  #("(", "("),
  #(")", ")"),
  #("[", "%5B"),
  #("@", "%40"),
  #("/", "%2F"),
  #("\\", "%5C"),
  #("&", "%26"),
  #("#", "%23"),
  #("=", "%3D"),
  #("~", "~"),
  #("ñ", "%C3%B1"),
  #("-", "-"),
  #("_", "_"),
  #(".", "."),
  #("*", "*"),
  #("+", "+"),
  #("100% great+fun", "100%25%20great+fun"),
]

// Allowed chars
pub fn percent_encode_test() {
  percent_codec_fixtures
  |> list.map(fn(t) {
    let #(a, b) = t
    assert uri.percent_encode(a) == b
  })
}

pub fn percent_encode_consistency_test() {
  let k = "weebl bob[]"
  let v = "ñaña (,:*~)"

  let query_string = uri.query_to_string([#(k, v)])

  let encoded_key = uri.percent_encode(k)
  let encoded_value = uri.percent_encode(v)
  let manual_query_string = string.concat([encoded_key, "=", encoded_value])

  assert query_string == manual_query_string
}

pub fn percent_decode_test() {
  percent_codec_fixtures
  |> list.map(fn(t) {
    let #(a, b) = t
    assert uri.percent_decode(b) == Ok(a)
  })
}

pub fn percent_decode_consistency_test() {
  let k = "weebl%20bob[]"
  let v = "%C3%B6rebro"
  let query = string.concat([k, "=", v])
  let assert Ok(parsed) = uri.parse_query(query)

  let assert Ok(decoded_key) = uri.percent_decode(k)
  let assert Ok(decoded_value) = uri.percent_decode(v)

  assert parsed == [#(decoded_key, decoded_value)]
}

pub fn parse_segments_test() {
  assert uri.path_segments("/") == []
  assert uri.path_segments("/weebl/bob") == ["weebl", "bob"]
  assert uri.path_segments("////") == []
  assert uri.path_segments("/weebl//bob") == ["weebl", "bob"]

  assert uri.path_segments("/.") == []
  assert uri.path_segments("/.weebl") == [".weebl"]

  assert uri.path_segments("/../bob") == ["bob"]
  assert uri.path_segments("../bob") == ["bob"]
  assert uri.path_segments("/weebl/../bob") == ["bob"]
}

pub fn origin1_test() {
  let assert Ok(parsed) = uri.parse("http://example.test/path?weebl#bob")
  assert uri.origin(parsed) == Ok("http://example.test")
}

pub fn origin2_test() {
  let assert Ok(parsed) = uri.parse("http://example.test:8080")
  assert uri.origin(parsed) == Ok("http://example.test:8080")
}

pub fn origin3_test() {
  let assert Ok(parsed) = uri.parse("https://example.test")
  assert uri.origin(parsed) == Ok("https://example.test")
}

pub fn origin4_test() {
  let assert Ok(parsed) = uri.parse("http:///path")
  assert uri.origin(parsed) == Ok("http://")
}

pub fn origin5_test() {
  let assert Ok(parsed) = uri.parse("http://")
  assert uri.origin(parsed) == Ok("http://")
}

pub fn origin6_test() {
  let assert Ok(parsed) = uri.parse("/path")
  assert uri.origin(parsed) == Error(Nil)
}

pub fn origin7_test() {
  let assert Ok(parsed) = uri.parse("file:///dev/null")
  assert uri.origin(parsed) == Error(Nil)
}

pub fn origin8_test() {
  let assert Ok(parsed) = uri.parse("https://mozilla.org:443/")
  assert uri.origin(parsed) == Ok("https://mozilla.org")
}

pub fn origin9_test() {
  let assert Ok(parsed) = uri.parse("http://localhost:80/")
  assert uri.origin(parsed) == Ok("http://localhost")
}

pub fn merge1_test() {
  let assert Ok(a) = uri.parse("/relative")
  let assert Ok(b) = uri.parse("")
  assert uri.merge(a, b) == Error(Nil)
}

pub fn merge2_test() {
  let assert Ok(a) = uri.parse("http://google.com/weebl")
  let assert Ok(b) = uri.parse("http://example.com/baz")
  assert uri.merge(a, b) == uri.parse("http://example.com/baz")
}

pub fn merge3_test() {
  let assert Ok(a) = uri.parse("http://google.com/weebl")
  let assert Ok(b) = uri.parse("http://example.com/.././bob/../../baz")
  assert uri.merge(a, b) == uri.parse("http://example.com/baz")
}

pub fn merge4_test() {
  let assert Ok(a) = uri.parse("http://google.com/weebl")
  let assert Ok(b) = uri.parse("//example.com/baz")
  assert uri.merge(a, b) == uri.parse("http://example.com/baz")
}

pub fn merge5_test() {
  let assert Ok(a) = uri.parse("http://google.com/weebl")
  let assert Ok(b) = uri.parse("//example.com/.././bob/../../../baz")
  assert uri.merge(a, b) == uri.parse("http://example.com/baz")
}

pub fn merge6_test() {
  let assert Ok(a) = uri.parse("http://example.com/weebl/bob")
  let assert Ok(b) = uri.parse("/baz")
  assert uri.merge(a, b) == uri.parse("http://example.com/baz")
}

pub fn merge7_test() {
  let assert Ok(a) = uri.parse("http://example.com/weebl/bob")
  let assert Ok(b) = uri.parse("baz")
  assert uri.merge(a, b) == uri.parse("http://example.com/weebl/baz")
}

pub fn merge8_test() {
  let assert Ok(a) = uri.parse("http://example.com/weebl/")
  let assert Ok(b) = uri.parse("baz")
  assert uri.merge(a, b) == uri.parse("http://example.com/weebl/baz")
}

pub fn merge9_test() {
  let assert Ok(a) = uri.parse("http://example.com")
  let assert Ok(b) = uri.parse("baz")
  assert uri.merge(a, b) == uri.parse("http://example.com/baz")
}

pub fn merge10_test() {
  let assert Ok(a) = uri.parse("http://example.com")
  let assert Ok(b) = uri.parse("/.././bob/../../../baz")
  assert uri.merge(a, b) == uri.parse("http://example.com/baz")
}

pub fn merge11_test() {
  let assert Ok(a) = uri.parse("http://example.com/weebl/bob")
  let assert Ok(b) = uri.parse("")
  assert uri.merge(a, b) == uri.parse("http://example.com/weebl/bob")
}

pub fn merge12_test() {
  let assert Ok(a) = uri.parse("http://example.com/weebl/bob")
  let assert Ok(b) = uri.parse("#fragment")
  assert uri.merge(a, b) == uri.parse("http://example.com/weebl/bob#fragment")
}

pub fn merge13_test() {
  let assert Ok(a) = uri.parse("http://example.com/weebl/bob")
  let assert Ok(b) = uri.parse("?query")
  assert uri.merge(a, b) == uri.parse("http://example.com/weebl/bob?query")
}

pub fn merge14_test() {
  let assert Ok(a) = uri.parse("http://example.com/weebl/bob?query1")
  let assert Ok(b) = uri.parse("?query2")
  assert uri.merge(a, b) == uri.parse("http://example.com/weebl/bob?query2")
}

pub fn merge15_test() {
  let assert Ok(a) = uri.parse("http://example.com/weebl/bob?query")
  let assert Ok(b) = uri.parse("")
  assert uri.merge(a, b) == uri.parse("http://example.com/weebl/bob?query")
}
