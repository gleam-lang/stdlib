import gleam/dict
import gleam/option.{None, Some}
import gleam/order
import gleam/should
import gleam/string

@target(erlang)
import gleam/int
@target(erlang)
import gleam/list
@target(erlang)
import gleam/result

pub fn length_test() {
  string.length("ß↑e̊")
  |> should.equal(3)

  string.length("Gleam")
  |> should.equal(5)

  string.length("")
  |> should.equal(0)
}

pub fn lowercase_test() {
  string.lowercase("Gleam")
  |> should.equal("gleam")
}

pub fn uppercase_test() {
  string.uppercase("Gleam")
  |> should.equal("GLEAM")
}

pub fn reverse_test() {
  "Gleam"
  |> string.reverse
  |> should.equal("maelG")

  " Gleam"
  |> string.reverse
  |> should.equal("maelG ")

  "👍 OK"
  |> string.reverse
  |> should.equal("KO 👍")

  "👍"
  |> string.reverse
  |> should.equal("👍")

  "ÅÄÖ"
  |> string.reverse
  |> should.equal("ÖÄÅ")

  "👶🏿"
  |> string.reverse
  |> should.equal("👶🏿")

  "👶🏿"
  |> string.reverse
  |> string.reverse
  |> should.equal("👶🏿")
}

pub fn split_test() {
  "Gleam,Erlang,Elixir"
  |> string.split(",")
  |> should.equal(["Gleam", "Erlang", "Elixir"])

  "Gleam, Erlang,Elixir"
  |> string.split(", ")
  |> should.equal(["Gleam", "Erlang,Elixir"])

  "Gleam On Beam"
  |> string.split("")
  |> should.equal([
    "G", "l", "e", "a", "m", " ", "O", "n", " ", "B", "e", "a", "m",
  ])
}

pub fn split_once_test() {
  "Gleam,Erlang,Elixir"
  |> string.split_once(",")
  |> should.equal(Ok(#("Gleam", "Erlang,Elixir")))

  "Gleam"
  |> string.split_once(",")
  |> should.equal(Error(Nil))

  ""
  |> string.split_once(",")
  |> should.equal(Error(Nil))
}

pub fn replace_test() {
  "Gleam,Erlang,Elixir"
  |> string.replace(",", "++")
  |> should.equal("Gleam++Erlang++Elixir")
}

pub fn append_test() {
  "Test"
  |> string.append(" Me")
  |> should.equal("Test Me")
}

pub fn compare_test() {
  string.compare("", "")
  |> should.equal(order.Eq)

  string.compare("a", "")
  |> should.equal(order.Gt)

  string.compare("a", "A")
  |> should.equal(order.Gt)

  string.compare("A", "B")
  |> should.equal(order.Lt)

  string.compare("t", "ABC")
  |> should.equal(order.Gt)
}

pub fn contains_test() {
  "gleam"
  |> string.contains("ea")
  |> should.be_true

  "gleam"
  |> string.contains("x")
  |> should.be_false

  string.contains(does: "bellwether", contain: "bell")
  |> should.be_true
}

pub fn concat_test() {
  ["Hello", ", ", "world!"]
  |> string.concat
  |> should.equal("Hello, world!")
}

pub fn concat_emoji_test() {
  ["💃🏿", "💇🏼‍♀️", "🧔‍♂️", "🧑‍🦼‍➡️"]
  |> string.concat
  |> should.equal("💃🏿💇🏼‍♀️🧔‍♂️🧑‍🦼‍➡️")
}

pub fn repeat_test() {
  "hi"
  |> string.repeat(times: 3)
  |> should.equal("hihihi")

  "hi"
  |> string.repeat(0)
  |> should.equal("")

  "hi"
  |> string.repeat(-1)
  |> should.equal("")
}

pub fn join_0_test() {
  []
  |> string.join(with: ", ")
  |> should.equal("")

  []
  |> string.join(with: "-")
  |> should.equal("")
}

pub fn join_1_test() {
  ["Hello"]
  |> string.join(with: ", ")
  |> should.equal("Hello")

  ["Hello"]
  |> string.join(with: "-")
  |> should.equal("Hello")
}

pub fn join_2_test() {
  ["Hello", "world!"]
  |> string.join(with: ", ")
  |> should.equal("Hello, world!")

  ["Hello", "world!"]
  |> string.join(with: "-")
  |> should.equal("Hello-world!")
}

pub fn join_3_test() {
  ["Hello", "there", "world!"]
  |> string.join(with: ", ")
  |> should.equal("Hello, there, world!")

  ["Hello", "there", "world!"]
  |> string.join(with: "-")
  |> should.equal("Hello-there-world!")
}

pub fn trim_test() {
  "  hats  \n"
  |> string.trim
  |> should.equal("hats")
}

pub fn trim2_test() {
  "k\r1=v2"
  |> string.trim
  |> should.equal("k\r1=v2")
}

pub fn trim3_test() {
  "  \nhello\nworld\n  "
  |> string.trim
  |> should.equal("hello\nworld")
}

pub fn trim_start_test() {
  "  hats  \n"
  |> string.trim_start
  |> should.equal("hats  \n")
}

pub fn trim_end_test() {
  "  hats  \n"
  |> string.trim_end
  |> should.equal("  hats")
}

pub fn trim_whole_string_test() {
  let s =
    "\u{0020}\u{0009}\u{000A}\u{000B}\u{000C}\u{000D}\u{0085}\u{2028}\u{2029}"

  s
  |> string.trim_start
  |> should.equal("")

  s
  |> string.trim_end
  |> should.equal("")

  s
  |> string.trim
  |> should.equal("")
}

// unicode whitespaces
pub fn trim_horizontal_tab_test() {
  "hats\u{0009}"
  |> string.trim
  |> should.equal("hats")
}

pub fn trim_newline_test() {
  "hats\u{000A}"
  |> string.trim
  |> should.equal("hats")
}

pub fn trim_vertical_tab_test() {
  "hats\u{000B}"
  |> string.trim
  |> should.equal("hats")
}

pub fn trim_form_feed_test() {
  "hats\u{000C}"
  |> string.trim
  |> should.equal("hats")
}

pub fn trim_carriage_return_test() {
  "hats\u{000D}"
  |> string.trim
  |> should.equal("hats")
}

pub fn trim_space_test() {
  "hats\u{0020}"
  |> string.trim
  |> should.equal("hats")
}

pub fn trim_no_break_space_test() {
  "hats\u{00A0}"
  |> string.trim
  |> should.equal("hats\u{00A0}")
}

pub fn trim_ogham_space_mark_test() {
  "hats\u{1680}"
  |> string.trim
  |> should.equal("hats\u{1680}")
}

pub fn trim_en_quad_test() {
  "hats\u{2000}"
  |> string.trim
  |> should.equal("hats\u{2000}")
}

pub fn trim_em_quad_test() {
  "hats\u{2001}"
  |> string.trim
  |> should.equal("hats\u{2001}")
}

pub fn trim_en_space_test() {
  "hats\u{2002}"
  |> string.trim
  |> should.equal("hats\u{2002}")
}

pub fn trim_em_space_test() {
  "hats\u{2003}"
  |> string.trim
  |> should.equal("hats\u{2003}")
}

pub fn trim_three_per_em_space_test() {
  "hats\u{2004}"
  |> string.trim
  |> should.equal("hats\u{2004}")
}

pub fn trim_four_per_em_space_test() {
  "hats\u{2005}"
  |> string.trim
  |> should.equal("hats\u{2005}")
}

pub fn trim_six_per_em_space_test() {
  "hats\u{2006}"
  |> string.trim
  |> should.equal("hats\u{2006}")
}

pub fn trim_figure_space_test() {
  "hats\u{2007}"
  |> string.trim
  |> should.equal("hats\u{2007}")
}

pub fn trim_punctuation_space_test() {
  "hats\u{2008}"
  |> string.trim
  |> should.equal("hats\u{2008}")
}

pub fn trim_thin_space_test() {
  "hats\u{2009}"
  |> string.trim
  |> should.equal("hats\u{2009}")
}

pub fn trim_hair_space_test() {
  "hats\u{200A}"
  |> string.trim
  |> should.equal("hats\u{200A}")
}

pub fn trim_line_separator_test() {
  "hats\u{2028}"
  |> string.trim
  |> should.equal("hats")
}

pub fn trim_paragraph_separator_test() {
  "hats\u{2029}"
  |> string.trim
  |> should.equal("hats")
}

pub fn trim_narrow_no_break_space_test() {
  "hats\u{202F}"
  |> string.trim
  |> should.equal("hats\u{202F}")
}

pub fn trim_medium_mathematical_space_test() {
  "hats\u{205F}"
  |> string.trim
  |> should.equal("hats\u{205F}")
}

pub fn trim_ideographic_space_test() {
  "hats\u{3000}"
  |> string.trim
  |> should.equal("hats\u{3000}")
}

// related unicode non-whitespaces
pub fn trim_mongolian_vowel_separator_test() {
  "hats\u{180E}"
  |> string.trim
  |> should.equal("hats\u{180E}")
}

pub fn trim_zero_width_space_test() {
  "hats\u{200B}"
  |> string.trim
  |> should.equal("hats\u{200B}")
}

pub fn trim_zero_width_non_joiner_test() {
  "hats\u{200C}"
  |> string.trim
  |> should.equal("hats\u{200C}")
}

pub fn trim_zero_width_joiner_test() {
  "hats\u{200D}"
  |> string.trim
  |> should.equal("hats\u{200D}")
}

pub fn trim_word_joiner_test() {
  "hats\u{2060}"
  |> string.trim
  |> should.equal("hats\u{2060}")
}

pub fn trim_zero_width_non_breaking_space_test() {
  "hats\u{FEFF}"
  |> string.trim
  |> should.equal("hats\u{FEFF}")
}

pub fn trim_comma_test() {
  "hats,"
  |> string.trim
  |> should.equal("hats,")
}

pub fn starts_with_test() {
  "theory"
  |> string.starts_with("")
  |> should.be_true

  "theory"
  |> string.starts_with("the")
  |> should.be_true

  "theory"
  |> string.starts_with("ory")
  |> should.be_false

  "theory"
  |> string.starts_with("theory2")
  |> should.be_false
}

pub fn ends_with_test() {
  "theory"
  |> string.ends_with("")
  |> should.be_true

  "theory"
  |> string.ends_with("ory")
  |> should.be_true

  "theory"
  |> string.ends_with("the")
  |> should.be_false

  "theory"
  |> string.ends_with("theory2")
  |> should.be_false
}

pub fn slice_test() {
  "gleam"
  |> string.slice(at_index: 1, length: 2)
  |> should.equal("le")

  "gleam"
  |> string.slice(at_index: 1, length: 10)
  |> should.equal("leam")

  "gleam"
  |> string.slice(at_index: 10, length: 3)
  |> should.equal("")

  "gleam"
  |> string.slice(at_index: -2, length: 2)
  |> should.equal("am")

  "gleam"
  |> string.slice(at_index: -12, length: 2)
  |> should.equal("")

  "gleam"
  |> string.slice(at_index: 2, length: -3)
  |> should.equal("")

  "👶🏿"
  |> string.slice(at_index: 0, length: 3)
  |> should.equal("👶🏿")
}

pub fn crop_test() {
  "gleam"
  |> string.crop("gl")
  |> should.equal("gleam")

  "gleam"
  |> string.crop("le")
  |> should.equal("leam")

  string.crop(from: "gleam", before: "ea")
  |> should.equal("eam")

  "gleam"
  |> string.crop("")
  |> should.equal("gleam")

  "gleam"
  |> string.crop("!")
  |> should.equal("gleam")
}

pub fn drop_start_test() {
  "gleam"
  |> string.drop_start(up_to: 2)
  |> should.equal("eam")

  "gleam"
  |> string.drop_start(up_to: 6)
  |> should.equal("")

  "gleam"
  |> string.drop_start(up_to: -2)
  |> should.equal("gleam")
}

pub fn drop_start_3499_test() {
  // https://github.com/gleam-lang/gleam/issues/3499
  "\r]"
  |> string.drop_start(1)
  |> should.equal("]")
}

pub fn drop_end_test() {
  "gleam"
  |> string.drop_end(up_to: 2)
  |> should.equal("gle")

  "gleam"
  |> string.drop_end(up_to: 5)
  |> should.equal("")

  "gleam"
  |> string.drop_end(up_to: -2)
  |> should.equal("gleam")
}

pub fn pad_start_test() {
  "121"
  |> string.pad_start(to: 5, with: ".")
  |> should.equal("..121")

  "121"
  |> string.pad_start(to: 3, with: ".")
  |> should.equal("121")

  "121"
  |> string.pad_start(to: 2, with: ".")
  |> should.equal("121")

  "121"
  |> string.pad_start(to: 4, with: "XY")
  |> should.equal("X121")

  "121"
  |> string.pad_start(to: 5, with: "XY")
  |> should.equal("XY121")

  "121"
  |> string.pad_start(to: 6, with: "XY")
  |> should.equal("XYX121")
}

pub fn pad_end_test() {
  "121"
  |> string.pad_end(to: 5, with: ".")
  |> should.equal("121..")

  "121"
  |> string.pad_end(to: 3, with: ".")
  |> should.equal("121")

  "121"
  |> string.pad_end(to: 2, with: ".")
  |> should.equal("121")

  "121"
  |> string.pad_end(to: 4, with: "XY")
  |> should.equal("121X")

  "121"
  |> string.pad_end(to: 5, with: "XY")
  |> should.equal("121XY")

  "121"
  |> string.pad_end(to: 6, with: "XY")
  |> should.equal("121XYX")
}

pub fn pop_grapheme_test() {
  "gleam"
  |> string.pop_grapheme
  |> should.equal(Ok(#("g", "leam")))

  "g"
  |> string.pop_grapheme
  |> should.equal(Ok(#("g", "")))

  ""
  |> string.pop_grapheme
  |> should.equal(Error(Nil))
}

pub fn to_graphemes_test() {
  ""
  |> string.to_graphemes
  |> should.equal([])

  "\n\t\r\"\\"
  |> string.to_graphemes
  |> should.equal(["\n", "\t", "\r", "\"", "\\"])

  "a"
  |> string.to_graphemes
  |> should.equal(["a"])

  "abc"
  |> string.to_graphemes
  |> should.equal(["a", "b", "c"])

  "🌷🎁💩😜👍🏳️‍🌈"
  |> string.to_graphemes
  |> should.equal(["🌷", "🎁", "💩", "😜", "👍", "🏳️‍🌈"])

  "Ĺo͂řȩm̅"
  |> string.to_graphemes
  |> should.equal(["Ĺ", "o͂", "ř", "ȩ", "m̅"])

  "뎌쉐"
  |> string.to_graphemes
  |> should.equal(["뎌", "쉐"])

  "👨‍👩‍👦‍👦"
  |> string.to_graphemes()
  |> should.equal(["👨‍👩‍👦‍👦"])

  "ごん゙に゙ぢば"
  |> string.to_graphemes()
  |> should.equal(["ご", "ん゙", "に゙", "ぢ", "ば"])

  "パピプペポ"
  |> string.to_graphemes()
  |> should.equal(["パ", "ピ", "プ", "ペ", "ポ"])

  "Z͑ͫ̓ͪ̂ͫ̽͏̴̙̤̞͉͚̯̞̠͍A̴̵̜̰͔ͫ͗͢L̠ͨͧͩ͘G̴̻͈͍͔̹̑͗̎̅͛́Ǫ̵̹̻̝̳͂̌̌͘!͖̬̰̙̗̿̋ͥͥ̂ͣ̐́́͜͞"
  |> string.to_graphemes
  |> should.equal(["Z͑ͫ̓ͪ̂ͫ̽͏̴̙̤̞͉͚̯̞̠͍", "A̴̵̜̰͔ͫ͗͢", "L̠ͨͧͩ͘", "G̴̻͈͍͔̹̑͗̎̅͛́", "Ǫ̵̹̻̝̳͂̌̌͘", "!͖̬̰̙̗̿̋ͥͥ̂ͣ̐́́͜͞"])
}

pub fn to_utf_codepoints_test() {
  ""
  |> string.to_utf_codepoints
  |> should.equal([])

  "gleam"
  |> string.to_utf_codepoints
  |> should.equal({
    let assert #(Ok(g), Ok(l), Ok(e), Ok(a), Ok(m)) = #(
      string.utf_codepoint(103),
      string.utf_codepoint(108),
      string.utf_codepoint(101),
      string.utf_codepoint(97),
      string.utf_codepoint(109),
    )
    [g, l, e, a, m]
  })

  // ["🏳", "️", "‍", "🌈"]
  let expected = {
    let assert #(
      Ok(waving_white_flag),
      Ok(variant_selector_16),
      Ok(zero_width_joiner),
      Ok(rainbow),
    ) = #(
      string.utf_codepoint(127_987),
      string.utf_codepoint(65_039),
      string.utf_codepoint(8205),
      string.utf_codepoint(127_752),
    )
    [waving_white_flag, variant_selector_16, zero_width_joiner, rainbow]
  }

  "🏳️‍🌈"
  |> string.to_utf_codepoints
  |> should.equal(expected)
}

pub fn from_utf_codepoints_test() {
  ""
  |> string.to_utf_codepoints
  |> string.from_utf_codepoints
  |> should.equal("")

  "gleam"
  |> string.to_utf_codepoints
  |> string.from_utf_codepoints
  |> should.equal("gleam")

  "🏳️‍🌈"
  |> string.to_utf_codepoints
  |> string.from_utf_codepoints
  |> should.equal("🏳️‍🌈")

  {
    let assert #(Ok(a), Ok(b), Ok(c)) = #(
      string.utf_codepoint(97),
      string.utf_codepoint(98),
      string.utf_codepoint(99),
    )
    [a, b, c]
  }
  |> string.from_utf_codepoints
  |> should.equal("abc")
}

pub fn utf_codepoint_test() {
  // Less than the lower bound on valid codepoints
  string.utf_codepoint(-1)
  |> should.be_error

  // The lower bound on valid codepoints
  string.utf_codepoint(0)
  |> should.be_ok

  // The upper bound for valid code points
  string.utf_codepoint(1_114_111)
  |> should.be_ok

  // Greater than the upper bound on valid codepoints
  string.utf_codepoint(1_114_112)
  |> should.be_error

  // Non-characters U+FFFE and U+FFFF are valid codepoints.  See (#778).
  string.utf_codepoint(65_534)
  |> should.be_ok
  string.utf_codepoint(65_535)
  |> should.be_ok

  // One less than the lowest "High-surrogate code point"
  string.utf_codepoint(55_295)
  |> should.be_ok

  // Lowest value of the "High-surrogate code point" (U+D800 to U+DBFF)
  string.utf_codepoint(55_296)
  |> should.be_error

  // Highest value of the "Low-surrogate code point" (U+DC00 to U+DFFF)
  string.utf_codepoint(57_343)
  |> should.be_error

  // One greater than the highest "Low-surrogate code point"
  string.utf_codepoint(57_344)
  |> should.be_ok
}

pub fn bit_array_utf_codepoint_test() {
  let assert Ok(snake) = string.utf_codepoint(128_013)
  should.equal(<<snake:utf8_codepoint>>, <<"🐍":utf8>>)
}

pub fn utf_codepoint_to_int_test() {
  {
    let assert Ok(ordinal_value) = string.utf_codepoint(128_013)
    ordinal_value
  }
  |> string.utf_codepoint_to_int
  |> should.equal(128_013)
}

pub fn to_option_test() {
  ""
  |> string.to_option
  |> should.equal(None)

  "ok"
  |> string.to_option
  |> should.equal(Some("ok"))
}

pub fn first_test() {
  ""
  |> string.first
  |> should.be_error

  "gleam"
  |> string.first
  |> should.equal(Ok("g"))

  "⭐️ Gleam"
  |> string.first
  |> should.equal(Ok("⭐️"))

  "a"
  |> string.first
  |> should.equal(Ok("a"))
}

pub fn last_test() {
  ""
  |> string.last
  |> should.be_error

  "gleam"
  |> string.last
  |> should.equal(Ok("m"))

  "gleam "
  |> string.last
  |> should.equal(Ok(" "))

  "եոգլի"
  |> string.last
  |> should.equal(Ok("ի"))

  "a"
  |> string.last
  |> should.equal(Ok("a"))
}

pub fn capitalise_test() {
  ""
  |> string.capitalise
  |> should.equal("")

  "gleam"
  |> string.capitalise
  |> should.equal("Gleam")

  "GLEAM"
  |> string.capitalise
  |> should.equal("Gleam")

  "g l e a m"
  |> string.capitalise
  |> should.equal("G l e a m")

  "1GLEAM"
  |> string.capitalise
  |> should.equal("1gleam")

  "_gLeAm1"
  |> string.capitalise
  |> should.equal("_gleam1")

  " gLeAm1"
  |> string.capitalise
  |> should.equal(" gleam1")

  "る"
  |> string.capitalise
  |> should.equal("る")
}

type InspectType(a, b) {
  InspectTypeZero
  InspectTypeOne(a)
  InspectTypeTwo(a, b)
}

pub fn inspect_test() {
  string.inspect(True)
  |> should.equal("True")

  string.inspect(False)
  |> should.equal("False")

  string.inspect([True, False])
  |> should.equal("[True, False]")

  string.inspect([False, False])
  |> should.equal("[False, False]")

  string.inspect([True, True])
  |> should.equal("[True, True]")

  string.inspect([Nil, Nil])
  |> should.equal("[Nil, Nil]")

  string.inspect(#(True, False))
  |> should.equal("#(True, False)")

  string.inspect(#(False, False))
  |> should.equal("#(False, False)")

  string.inspect(#(True, True))
  |> should.equal("#(True, True)")

  string.inspect(#(Nil, True))
  |> should.equal("#(Nil, True)")

  string.inspect(#(Nil, False))
  |> should.equal("#(Nil, False)")

  string.inspect(#(True, Nil))
  |> should.equal("#(True, Nil)")

  string.inspect(#(False, Nil))
  |> should.equal("#(False, Nil)")

  string.inspect(-1)
  |> should.equal("-1")

  string.inspect(0)
  |> should.equal("0")

  string.inspect(1)
  |> should.equal("1")

  string.inspect([])
  |> should.equal("[]")

  string.inspect([1])
  |> should.equal("[1]")

  string.inspect([1, 2])
  |> should.equal("[1, 2]")

  string.inspect([[1], [1]])
  |> should.equal("[[1], [1]]")

  string.inspect(-1.5)
  |> should.equal("-1.5")

  string.inspect(5.0e-26)
  |> should.equal("5.0e-26")

  string.inspect(1.5)
  |> should.equal("1.5")

  string.inspect(-5.0e-26)
  |> should.equal("-5.0e-26")

  string.inspect([1.5])
  |> should.equal("[1.5]")

  string.inspect("")
  |> should.equal("\"\"")

  string.inspect("\\")
  |> should.equal("\"\\\\\"")

  string.inspect("\\\\")
  |> should.equal("\"\\\\\\\\\"")

  string.inspect("\\\\\\")
  |> should.equal("\"\\\\\\\\\\\\\"")

  string.inspect("\"")
  |> should.equal("\"\\\"\"")
  string.inspect("\"\"")
  |> should.equal("\"\\\"\\\"\"")

  string.inspect("\r")
  |> should.equal("\"\\r\"")

  string.inspect("\n")
  |> should.equal("\"\\n\"")

  string.inspect("\t")
  |> should.equal("\"\\t\"")

  string.inspect("\f")
  |> should.equal("\"\\f\"")

  string.inspect("\u{0008}")
  |> should.equal("\"\\u{0008}\"")

  string.inspect("\u{000B}")
  |> should.equal("\"\\u{000B}\"")

  string.inspect("\u{001B}")
  |> should.equal("\"\\u{001B}\"")

  string.inspect("\u{0015}")
  |> should.equal("\"\\u{0015}\"")

  string.inspect("\u{001F}")
  |> should.equal("\"\\u{001F}\"")

  string.inspect("\u{0020}")
  |> should.equal("\" \"")

  string.inspect("\u{007F}")
  |> should.equal("\"\\u{007F}\"")

  string.inspect("\u{009F}")
  |> should.equal("\"\\u{009F}\"")

  string.inspect("\u{00A0}")
  |> should.equal("\"\u{00A0}\"")

  string.inspect("\r\r")
  |> should.equal("\"\\r\\r\"")

  string.inspect("\n\n")
  |> should.equal("\"\\n\\n\"")

  string.inspect("\r\n")
  |> should.equal("\"\\r\\n\"")

  string.inspect("\n\r")
  |> should.equal("\"\\n\\r\"")

  string.inspect("\t\t")
  |> should.equal("\"\\t\\t\"")

  string.inspect("\t\n")
  |> should.equal("\"\\t\\n\"")

  string.inspect("\n\t")
  |> should.equal("\"\\n\\t\"")

  string.inspect("\t\r")
  |> should.equal("\"\\t\\r\"")

  string.inspect("\r\t")
  |> should.equal("\"\\r\\t\"")

  string.inspect("\t\f")
  |> should.equal("\"\\t\\f\"")

  string.inspect("\f\t")
  |> should.equal("\"\\f\\t\"")

  string.inspect("\t\u{0008}")
  |> should.equal("\"\\t\\u{0008}\"")

  string.inspect("\u{0008}\t")
  |> should.equal("\"\\u{0008}\\t\"")

  string.inspect("\t\u{000B}")
  |> should.equal("\"\\t\\u{000B}\"")

  string.inspect("\u{000B}\t")
  |> should.equal("\"\\u{000B}\\t\"")

  string.inspect("\t\u{001B}")
  |> should.equal("\"\\t\\u{001B}\"")

  string.inspect("\u{001B}\t")
  |> should.equal("\"\\u{001B}\\t\"")

  string.inspect("\\\n\\")
  |> should.equal("\"\\\\\\n\\\\\"")

  string.inspect("\\\"\\")
  |> should.equal("\"\\\\\\\"\\\\\"")

  string.inspect("\\\"\"\\")
  |> should.equal("\"\\\\\\\"\\\"\\\\\"")

  string.inspect("'")
  |> should.equal("\"'\"")

  string.inspect("''")
  |> should.equal("\"''\"")

  string.inspect("around-single-quotes'around-single-quotes")
  |> should.equal("\"around-single-quotes'around-single-quotes\"")

  string.inspect("'between-single-quotes'")
  |> should.equal("\"'between-single-quotes'\"")

  string.inspect("0")
  |> should.equal("\"0\"")

  string.inspect("1")
  |> should.equal("\"1\"")

  string.inspect("2")
  |> should.equal("\"2\"")

  string.inspect("Hello Joe!")
  |> should.equal("\"Hello Joe!\"")

  string.inspect("Hello \"Manuel\"!")
  |> should.equal("\"Hello \\\"Manuel\\\"!\"")

  string.inspect("👨‍👩‍👦‍👦 💜 Gleam")
  |> should.equal("\"👨‍👩‍👦‍👦 💜 Gleam\"")

  string.inspect("✨")
  |> should.equal("\"✨\"")

  string.inspect("🏳️‍⚧️")
  |> should.equal("\"🏳️‍⚧️\"")

  string.inspect("True")
  |> should.equal("\"True\"")

  string.inspect("False")
  |> should.equal("\"False\"")

  string.inspect("Nil")
  |> should.equal("\"Nil\"")

  string.inspect(["1"])
  |> should.equal("[\"1\"]")

  string.inspect(#())
  |> should.equal("#()")

  string.inspect(#(1))
  |> should.equal("#(1)")

  string.inspect(#("1"))
  |> should.equal("#(\"1\")")

  string.inspect(#(1.5))
  |> should.equal("#(1.5)")

  string.inspect([#(1, 2, 3), #(1, 2, 3)])
  |> should.equal("[#(1, 2, 3), #(1, 2, 3)]")

  string.inspect(#([1, 2, 3], "🌈", "🏳️‍🌈", #(1, "1", True)))
  |> should.equal("#([1, 2, 3], \"🌈\", \"🏳️‍🌈\", #(1, \"1\", True))")

  string.inspect(Nil)
  |> should.equal("Nil")

  string.inspect(Ok(1))
  |> should.equal("Ok(1)")

  string.inspect(Ok(True))
  |> should.equal("Ok(True)")

  string.inspect(Ok(False))
  |> should.equal("Ok(False)")

  string.inspect(Ok(Nil))
  |> should.equal("Ok(Nil)")

  string.inspect(Error(2))
  |> should.equal("Error(2)")

  string.inspect(Error(True))
  |> should.equal("Error(True)")

  string.inspect(Error(False))
  |> should.equal("Error(False)")

  string.inspect(Error(Nil))
  |> should.equal("Error(Nil)")

  string.inspect(InspectTypeZero)
  |> should.equal("InspectTypeZero")

  string.inspect(InspectTypeOne(1))
  |> should.equal("InspectTypeOne(1)")

  string.inspect(InspectTypeTwo(1, 2))
  |> should.equal("InspectTypeTwo(1, 2)")

  string.inspect(InspectTypeOne([1]))
  |> should.equal("InspectTypeOne([1])")

  string.inspect(InspectTypeOne("1"))
  |> should.equal("InspectTypeOne(\"1\")")

  string.inspect(InspectTypeOne(["1"]))
  |> should.equal("InspectTypeOne([\"1\"])")

  string.inspect(InspectTypeOne(#([1], "a")))
  |> should.equal("InspectTypeOne(#([1], \"a\"))")

  string.inspect(Ok)
  |> should.equal("//fn(a) { ... }")

  string.inspect(Error)
  |> should.equal("//fn(a) { ... }")

  string.inspect(fn() { Nil })
  |> should.equal("//fn() { ... }")

  string.inspect(fn(_) { Nil })
  |> should.equal("//fn(a) { ... }")

  string.inspect(fn(_, _) { Nil })
  |> should.equal("//fn(a, b) { ... }")

  string.inspect(fn(_, _) { Nil })
  |> should.equal("//fn(a, b) { ... }")

  string.inspect(fn(_: Int, _: String) -> Bool { False })
  |> should.equal("//fn(a, b) { ... }")

  string.inspect(#(InspectTypeOne, InspectTypeTwo))
  |> should.equal("#(//fn(a) { ... }, //fn(a, b) { ... })")

  string.inspect(InspectTypeOne(InspectTypeZero))
  |> should.equal("InspectTypeOne(InspectTypeZero)")

  string.inspect(<<255, 2, 0>>)
  |> should.equal("<<255, 2, 0>>")
}

pub fn inspect_charlist_test() {
  let list = [
    70, 97, 105, 108, 101, 100, 32, 116, 111, 32, 108, 111, 97, 100, 32, 78, 73,
    70, 32, 108, 105, 98, 114, 97, 114, 121, 58, 32, 39, 47, 114, 117, 110, 47,
    99, 117, 114, 114, 101, 110, 116, 45, 115, 121, 115, 116, 101, 109, 47, 115,
    119, 47, 115, 104, 97, 114, 101, 47, 110, 105, 120, 45, 108, 100, 47, 108,
    105, 98, 47, 108, 105, 98, 99, 114, 121, 112, 116, 111, 46, 115, 111, 46, 51,
  ]
  string.inspect(list)
  |> should.equal(
    "charlist.from_string(\"Failed to load NIF library: '/run/current-system/sw/share/nix-ld/lib/libcrypto.so.3\")",
  )
}

@target(javascript)
pub fn target_inspect_test() {
  // Due to Erlang's internal representation, on Erlang this passes, instead:
  // string.inspect(#(InspectTypeZero, InspectTypeZero))
  // |> should.equal("InspectTypeZero(InspectTypeZero)")
  string.inspect(#(InspectTypeZero, InspectTypeZero))
  |> should.equal("#(InspectTypeZero, InspectTypeZero)")

  // Due to JavaScript's `Number` type `Float`s without digits return as
  // `Int`s.
  string.inspect(-1.0)
  |> should.equal("-1")

  string.inspect(0.0)
  |> should.equal("0")

  string.inspect(1.0)
  |> should.equal("1")

  string.inspect([1.0])
  |> should.equal("[1]")

  string.inspect(#(1.0))
  |> should.equal("#(1)")

  // Unlike on Erlang, on JavaScript `BitArray` and `String` do have a
  // different runtime representation.
  <<"abc":utf8>>
  |> string.inspect()
  |> should.equal("<<97, 98, 99>>")
}

@target(erlang)
import gleam/dynamic.{type Dynamic}

// Test inspect on Erlang atoms valid and invalid in Gleam

@target(erlang)
@external(erlang, "erlang", "self")
fn create_erlang_pid() -> String

@target(erlang)
@external(erlang, "erlang", "make_ref")
fn create_erlang_reference() -> String

@target(erlang)
pub fn target_inspect_test() {
  // Erlang's internal representation does not allow a correct
  // differentiation at runtime and thus this does not pass:
  // string.inspect(#(InspectTypeZero, InspectTypeZero))
  // |> should.equal("#(InspectTypeZero, InspectTypeZero)")
  string.inspect(#(InspectTypeZero, InspectTypeZero))
  |> should.equal("InspectTypeZero(InspectTypeZero)")

  // Unlike JavaScript, Erlang correctly differentiates between `1` and `1.0`
  // at runtime.
  string.inspect(-1.0)
  |> should.equal("-1.0")

  string.inspect(0.0)
  |> should.equal("0.0")

  string.inspect(1.0)
  |> should.equal("1.0")

  string.inspect([1.0])
  |> should.equal("[1.0]")

  string.inspect(#(1.0))
  |> should.equal("#(1.0)")

  // Looks like `//erl(<0.83.0>)`.
  string.inspect(create_erlang_pid())
  |> looks_like_pid
  |> should.be_true

  // Looks like: `//erl(#Ref<0.1809744150.4035444737.100468>)`.
  string.inspect(create_erlang_reference())
  |> looks_like_ref
  |> should.be_true

  // On Erlang the representation between `String` and `BitArray` is
  // indistinguishable at runtime.
  <<"abc":utf8>>
  |> string.inspect()
  |> should.equal("\"abc\"")
}

@target(erlang)
fn looks_like_pid(string: String) -> Bool {
  case string {
    "//erl(<" <> string ->
      case string.ends_with(string, ">)") {
        False -> False
        True ->
          case string.drop_end(string, 2) |> string.split(on: ".") {
            [_, _, _] as numbers ->
              list.try_map(numbers, int.parse)
              |> result.is_ok

            _ -> False
          }
      }
    _ -> False
  }
}

@target(erlang)
fn looks_like_ref(string: String) -> Bool {
  case string {
    "//erl(#Ref<" <> string ->
      case string.ends_with(string, ">)") {
        False -> False
        True ->
          case string.drop_end(string, 2) |> string.split(on: ".") {
            [_, _, _, _] as numbers ->
              list.try_map(numbers, int.parse)
              |> result.is_ok

            _ -> False
          }
      }
    _ -> False
  }
}

@target(erlang)
pub fn improper_list_inspect_test() {
  let list = improper_list_append(1, 2, 3)
  let assert "//erl([1, 2 | 3])" = string.inspect(list)
}

// Warning: The type of this function is incorrect
@target(erlang)
@external(erlang, "gleam_stdlib_test_ffi", "improper_list_append")
fn improper_list_append(
  a: item_a,
  b: item_b,
  c: improper_tail,
) -> List(anything)

@target(erlang)
@external(erlang, "erlang", "binary_to_atom")
fn string_to_erlang_atom(a: String) -> Dynamic

@target(erlang)
pub fn inspect_erlang_atom_is_valid_in_gleam_test() {
  string_to_erlang_atom("one_two")
  |> string.inspect
  |> should.equal("OneTwo")

  string_to_erlang_atom("one1_two")
  |> string.inspect
  |> should.equal("One1Two")

  string_to_erlang_atom("one1two")
  |> string.inspect
  |> should.equal("One1two")
}

@target(erlang)
pub fn inspect_erlang_atom_with_a_leading_underscore_is_invalid_in_gleam_test() {
  string_to_erlang_atom("_ok")
  |> string.inspect
  |> should.equal("atom.create_from_string(\"_ok\")")
}

@target(erlang)
pub fn inspect_erlang_atom_with_a_trailing_underscore_is_invalid_in_gleam_test() {
  string_to_erlang_atom("ok_")
  |> string.inspect
  |> should.equal("atom.create_from_string(\"ok_\")")
}

@target(erlang)
pub fn inspect_erlang_atom_with_a_double_underscore_is_invalid_in_gleam_test() {
  string_to_erlang_atom("ok__ok")
  |> string.inspect
  |> should.equal("atom.create_from_string(\"ok__ok\")")
}

@target(erlang)
pub fn inspect_erlang_atom_with_white_spaces_is_invalid_in_gleam_test() {
  string_to_erlang_atom("ok ok")
  |> string.inspect
  |> should.equal("atom.create_from_string(\"ok ok\")")
}

@target(erlang)
pub fn inspect_erlang_atom_that_is_an_empty_string_is_invalid_in_gleam_test() {
  // An empty string based atom is invalid in gleam
  string_to_erlang_atom("")
  |> string.inspect
  |> should.equal("atom.create_from_string(\"\")")
}

@target(erlang)
pub fn inspect_erlang_atom_with_uppercases_invalid_in_gleam_test() {
  string_to_erlang_atom("Upper")
  |> string.inspect
  |> should.equal("atom.create_from_string(\"Upper\")")
}

@target(erlang)
pub fn inspect_erlang_atom_tag_tuple_test() {
  #(string_to_erlang_atom("DOWN"), 1, 2)
  |> string.inspect
  |> should.equal("#(atom.create_from_string(\"DOWN\"), 1, 2)")
}

@target(erlang)
pub fn inspect_erlang_atom_with_leading_digit_invalid_in_gleam_test() {
  string_to_erlang_atom("1_ok")
  |> string.inspect
  |> should.equal("atom.create_from_string(\"1_ok\")")

  string_to_erlang_atom("1Ok")
  |> string.inspect
  |> should.equal("atom.create_from_string(\"1Ok\")")
}

@target(erlang)
pub fn fifteen_bit_int_test() {
  <<2, 3:size(7)>>
  |> string.inspect
  |> should.equal("<<2, 3:size(7)>>")
}

pub fn byte_size_test() {
  let assert 0 = string.byte_size("")
  let assert 1 = string.byte_size("a")
  let assert 2 = string.byte_size("ab")
  let assert 3 = string.byte_size("abc")

  // Unicode graphemes. These will be multiple bytes.
  let assert 1 = string.byte_size("a")
  let assert 2 = string.byte_size("ä")
  let assert 4 = string.byte_size("👩")
  let assert 8 = string.byte_size("👩🏾")
  let assert 15 = string.byte_size("👩🏾‍🦰")
}

pub fn inspect_map_test() {
  dict.from_list([#("a", 1), #("b", 2)])
  |> string.inspect
  |> should.equal("dict.from_list([#(\"a\", 1), #(\"b\", 2)])")
}
