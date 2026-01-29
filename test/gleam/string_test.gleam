import gleam/dict
import gleam/option.{None, Some}
import gleam/order
import gleam/string

@target(erlang)
import gleam/int
@target(erlang)
import gleam/list
@target(erlang)
import gleam/result

@target(javascript)
import gleam/dynamic.{type Dynamic}

pub fn length_unicode_test() {
  assert string.length("ß↑e̊") == 3
}

pub fn length_basic_test() {
  assert string.length("Gleam") == 5
}

pub fn length_empty_test() {
  assert string.length("") == 0
}

pub fn lowercase_test() {
  assert string.lowercase("Gleam") == "gleam"
}

pub fn uppercase_test() {
  assert string.uppercase("Gleam") == "GLEAM"
}

pub fn reverse_basic_test() {
  assert string.reverse("Gleam") == "maelG"
}

pub fn reverse_with_space_test() {
  assert string.reverse(" Gleam") == "maelG "
}

pub fn reverse_emoji_test() {
  assert string.reverse("👍 OK") == "KO 👍"
}

pub fn reverse_single_emoji_test() {
  assert string.reverse("👍") == "👍"
}

pub fn reverse_unicode_test() {
  assert string.reverse("ÅÄÖ") == "ÖÄÅ"
}

pub fn reverse_skin_tone_emoji_test() {
  assert string.reverse("👶🏿") == "👶🏿"
}

pub fn reverse_skin_tone_roundtrip_test() {
  assert "👶🏿"
    |> string.reverse
    |> string.reverse
    == "👶🏿"
}

pub fn split_comma_test() {
  assert string.split("Gleam,Erlang,Elixir", ",")
    == ["Gleam", "Erlang", "Elixir"]
}

pub fn split_comma_space_test() {
  assert string.split("Gleam, Erlang,Elixir", ", ")
    == ["Gleam", "Erlang,Elixir"]
}

pub fn split_empty_test() {
  assert string.split("Gleam On Beam", "")
    == ["G", "l", "e", "a", "m", " ", "O", "n", " ", "B", "e", "a", "m"]
}

pub fn split_once_ok_test() {
  assert string.split_once("Gleam,Erlang,Elixir", ",")
    == Ok(#("Gleam", "Erlang,Elixir"))
}

pub fn split_once_not_found_test() {
  assert string.split_once("Gleam", ",") == Error(Nil)
}

pub fn split_once_empty_test() {
  assert string.split_once("", ",") == Error(Nil)
}

pub fn replace_test() {
  assert string.replace("Gleam,Erlang,Elixir", ",", "++")
    == "Gleam++Erlang++Elixir"
}

pub fn append_test() {
  assert string.append("Test", " Me") == "Test Me"
}

pub fn compare_equal_test() {
  assert string.compare("", "") == order.Eq
}

pub fn compare_a_gt_empty_test() {
  assert string.compare("a", "") == order.Gt
}

pub fn compare_lowercase_a_gt_uppercase_a_test() {
  assert string.compare("a", "A") == order.Gt
}

pub fn compare_uppercase_a_lt_uppercase_b_test() {
  assert string.compare("A", "B") == order.Lt
}

pub fn compare_t_gt_uppercase_abc_test() {
  assert string.compare("t", "ABC") == order.Gt
}

pub fn contains_present_test() {
  assert string.contains("gleam", "ea")
}

pub fn contains_absent_test() {
  assert !string.contains("gleam", "x")
}

pub fn contains_labelled_test() {
  assert string.contains(does: "bellwether", contain: "bell")
}

pub fn concat_test() {
  assert string.concat(["Hello", ", ", "world!"]) == "Hello, world!"
}

pub fn concat_emoji_test() {
  assert string.concat(["💃🏿", "💇🏼‍♀️", "🧔‍♂️", "🧑‍🦼‍➡️"]) == "💃🏿💇🏼‍♀️🧔‍♂️🧑‍🦼‍➡️"
}

pub fn repeat_one_test() {
  assert string.repeat("hi", times: 1) == "hi"
}

pub fn repeat_two_test() {
  assert string.repeat("hi", times: 2) == "hihi"
}

pub fn repeat_three_test() {
  assert string.repeat("hi", times: 3) == "hihihi"
}

pub fn repeat_large_test() {
  assert string.repeat("a", times: 10_001) |> string.length == 10_001
}

pub fn repeat_zero_test() {
  assert string.repeat("hi", 0) == ""
}

pub fn repeat_negative_test() {
  assert string.repeat("hi", -1) == ""
}

pub fn join_0_test() {
  assert string.join([], with: ", ") == ""
}

pub fn join_0_dash_test() {
  assert string.join([], with: "-") == ""
}

pub fn join_1_test() {
  assert string.join(["Hello"], with: ", ") == "Hello"
}

pub fn join_1_dash_test() {
  assert string.join(["Hello"], with: "-") == "Hello"
}

pub fn join_2_test() {
  assert string.join(["Hello", "world!"], with: ", ") == "Hello, world!"
}

pub fn join_2_dash_test() {
  assert string.join(["Hello", "world!"], with: "-") == "Hello-world!"
}

pub fn join_3_test() {
  assert string.join(["Hello", "there", "world!"], with: ", ")
    == "Hello, there, world!"
}

pub fn join_3_dash_test() {
  assert string.join(["Hello", "there", "world!"], with: "-")
    == "Hello-there-world!"
}

pub fn trim_test() {
  assert string.trim("  hats  \n") == "hats"
}

pub fn trim2_test() {
  assert string.trim("k\r1=v2") == "k\r1=v2"
}

pub fn trim3_test() {
  assert string.trim("  \nhello\nworld\n  ") == "hello\nworld"
}

pub fn trim_start_test() {
  assert string.trim_start("  hats  \n") == "hats  \n"
}

pub fn trim_end_test() {
  assert string.trim_end("  hats  \n") == "  hats"
}

pub fn trim_whole_string_start_test() {
  let s =
    "\u{0020}\u{0009}\u{000A}\u{000B}\u{000C}\u{000D}\u{0085}\u{2028}\u{2029}"

  assert string.trim_start(s) == ""
}

pub fn trim_whole_string_end_test() {
  let s =
    "\u{0020}\u{0009}\u{000A}\u{000B}\u{000C}\u{000D}\u{0085}\u{2028}\u{2029}"

  assert string.trim_end(s) == ""
}

pub fn trim_whole_string_test() {
  let s =
    "\u{0020}\u{0009}\u{000A}\u{000B}\u{000C}\u{000D}\u{0085}\u{2028}\u{2029}"

  assert string.trim(s) == ""
}

// unicode whitespaces
pub fn trim_horizontal_tab_test() {
  assert string.trim("hats\u{0009}") == "hats"
}

pub fn trim_newline_test() {
  assert string.trim("hats\u{000A}") == "hats"
}

pub fn trim_vertical_tab_test() {
  assert string.trim("hats\u{000B}") == "hats"
}

pub fn trim_form_feed_test() {
  assert string.trim("hats\u{000C}") == "hats"
}

pub fn trim_carriage_return_test() {
  assert string.trim("hats\u{000D}") == "hats"
}

pub fn trim_space_test() {
  assert string.trim("hats\u{0020}") == "hats"
}

pub fn trim_no_break_space_test() {
  assert string.trim("hats\u{00A0}") == "hats\u{00A0}"
}

pub fn trim_ogham_space_mark_test() {
  assert string.trim("hats\u{1680}") == "hats\u{1680}"
}

pub fn trim_en_quad_test() {
  assert string.trim("hats\u{2000}") == "hats\u{2000}"
}

pub fn trim_em_quad_test() {
  assert string.trim("hats\u{2001}") == "hats\u{2001}"
}

pub fn trim_en_space_test() {
  assert string.trim("hats\u{2002}") == "hats\u{2002}"
}

pub fn trim_em_space_test() {
  assert string.trim("hats\u{2003}") == "hats\u{2003}"
}

pub fn trim_three_per_em_space_test() {
  assert string.trim("hats\u{2004}") == "hats\u{2004}"
}

pub fn trim_four_per_em_space_test() {
  assert string.trim("hats\u{2005}") == "hats\u{2005}"
}

pub fn trim_six_per_em_space_test() {
  assert string.trim("hats\u{2006}") == "hats\u{2006}"
}

pub fn trim_figure_space_test() {
  assert string.trim("hats\u{2007}") == "hats\u{2007}"
}

pub fn trim_punctuation_space_test() {
  assert string.trim("hats\u{2008}") == "hats\u{2008}"
}

pub fn trim_thin_space_test() {
  assert string.trim("hats\u{2009}") == "hats\u{2009}"
}

pub fn trim_hair_space_test() {
  assert string.trim("hats\u{200A}") == "hats\u{200A}"
}

pub fn trim_line_separator_test() {
  assert string.trim("hats\u{2028}") == "hats"
}

pub fn trim_paragraph_separator_test() {
  assert string.trim("hats\u{2029}") == "hats"
}

pub fn trim_narrow_no_break_space_test() {
  assert string.trim("hats\u{202F}") == "hats\u{202F}"
}

pub fn trim_medium_mathematical_space_test() {
  assert string.trim("hats\u{205F}") == "hats\u{205F}"
}

pub fn trim_ideographic_space_test() {
  assert string.trim("hats\u{3000}") == "hats\u{3000}"
}

// related unicode non-whitespaces
pub fn trim_mongolian_vowel_separator_test() {
  assert string.trim("hats\u{180E}") == "hats\u{180E}"
}

pub fn trim_zero_width_space_test() {
  assert string.trim("hats\u{200B}") == "hats\u{200B}"
}

pub fn trim_zero_width_non_joiner_test() {
  assert string.trim("hats\u{200C}") == "hats\u{200C}"
}

pub fn trim_zero_width_joiner_test() {
  assert string.trim("hats\u{200D}") == "hats\u{200D}"
}

pub fn trim_word_joiner_test() {
  assert string.trim("hats\u{2060}") == "hats\u{2060}"
}

pub fn trim_zero_width_non_breaking_space_test() {
  assert string.trim("hats\u{FEFF}") == "hats\u{FEFF}"
}

pub fn trim_comma_test() {
  assert string.trim("hats,") == "hats,"
}

pub fn starts_with_empty_test() {
  assert string.starts_with("theory", "")
}

pub fn starts_with_match_test() {
  assert string.starts_with("theory", "the")
}

pub fn starts_with_no_match_test() {
  assert !string.starts_with("theory", "ory")
}

pub fn starts_with_longer_test() {
  assert !string.starts_with("theory", "theory2")
}

pub fn ends_with_empty_test() {
  assert string.ends_with("theory", "")
}

pub fn ends_with_match_test() {
  assert string.ends_with("theory", "ory")
}

pub fn ends_with_no_match_test() {
  assert !string.ends_with("theory", "the")
}

pub fn ends_with_longer_test() {
  assert !string.ends_with("theory", "theory2")
}

pub fn slice_basic_test() {
  assert string.slice("gleam", at_index: 1, length: 2) == "le"
}

pub fn slice_beyond_length_test() {
  assert string.slice("gleam", at_index: 1, length: 10) == "leam"
}

pub fn slice_beyond_string_test() {
  assert string.slice("gleam", at_index: 10, length: 3) == ""
}

pub fn slice_negative_index_test() {
  assert string.slice("gleam", at_index: -2, length: 2) == "am"
}

pub fn slice_negative_beyond_test() {
  assert string.slice("gleam", at_index: -12, length: 2) == ""
}

pub fn slice_negative_length_test() {
  assert string.slice("gleam", at_index: 2, length: -3) == ""
}

pub fn slice_zero_length_test() {
  assert string.slice("gleam", at_index: 2, length: 0) == ""
}

pub fn slice_emoji_test() {
  assert string.slice("👶🏿", at_index: 0, length: 3) == "👶🏿"
}

pub fn crop_start_test() {
  assert string.crop("gleam", "gl") == "gleam"
}

pub fn crop_middle_test() {
  assert string.crop("gleam", "le") == "leam"
}

pub fn crop_labelled_test() {
  assert string.crop(from: "gleam", before: "ea") == "eam"
}

pub fn crop_empty_test() {
  assert string.crop("gleam", "") == "gleam"
}

pub fn crop_not_found_test() {
  assert string.crop("gleam", "!") == "gleam"
}

pub fn drop_start_basic_test() {
  assert string.drop_start("gleam", up_to: 2) == "eam"
}

pub fn drop_start_beyond_length_test() {
  assert string.drop_start("gleam", up_to: 6) == ""
}

pub fn drop_start_negative_test() {
  assert string.drop_start("gleam", up_to: -2) == "gleam"
}

pub fn drop_start_zero_test() {
  assert string.drop_start("gleam", up_to: 0) == "gleam"
}

pub fn drop_start_3499_test() {
  // https://github.com/gleam-lang/gleam/issues/3499
  assert string.drop_start("\r]", 1) == "]"
}

pub fn drop_end_basic_test() {
  assert string.drop_end("gleam", up_to: 2) == "gle"
}

pub fn drop_end_all_test() {
  assert string.drop_end("gleam", up_to: 5) == ""
}

pub fn drop_end_negative_test() {
  assert string.drop_end("gleam", up_to: -2) == "gleam"
}

pub fn drop_end_zero_test() {
  assert string.drop_end("gleam", up_to: 0) == "gleam"
}

pub fn pad_start_basic_test() {
  assert string.pad_start("121", to: 5, with: ".") == "..121"
}

pub fn pad_start_exact_test() {
  assert string.pad_start("121", to: 3, with: ".") == "121"
}

pub fn pad_start_shorter_test() {
  assert string.pad_start("121", to: 2, with: ".") == "121"
}

pub fn pad_start_partial_multi_test() {
  assert string.pad_start("121", to: 4, with: "XY") == "X121"
}

pub fn pad_start_exact_multi_test() {
  assert string.pad_start("121", to: 5, with: "XY") == "XY121"
}

pub fn pad_start_extra_multi_test() {
  assert string.pad_start("121", to: 6, with: "XY") == "XYX121"
}

pub fn pad_end_basic_test() {
  assert string.pad_end("121", to: 5, with: ".") == "121.."
}

pub fn pad_end_exact_test() {
  assert string.pad_end("121", to: 3, with: ".") == "121"
}

pub fn pad_end_shorter_test() {
  assert string.pad_end("121", to: 2, with: ".") == "121"
}

pub fn pad_end_partial_multi_test() {
  assert string.pad_end("121", to: 4, with: "XY") == "121X"
}

pub fn pad_end_exact_multi_test() {
  assert string.pad_end("121", to: 5, with: "XY") == "121XY"
}

pub fn pad_end_extra_multi_test() {
  assert string.pad_end("121", to: 6, with: "XY") == "121XYX"
}

pub fn pop_grapheme_ok_test() {
  assert string.pop_grapheme("gleam") == Ok(#("g", "leam"))
}

pub fn pop_grapheme_single_test() {
  assert string.pop_grapheme("g") == Ok(#("g", ""))
}

pub fn pop_grapheme_empty_test() {
  assert string.pop_grapheme("") == Error(Nil)
}

pub fn to_graphemes_empty_test() {
  assert string.to_graphemes("") == []
}

pub fn to_graphemes_escape_chars_test() {
  assert string.to_graphemes("\n\t\r\"\\") == ["\n", "\t", "\r", "\"", "\\"]
}

pub fn to_graphemes_single_test() {
  assert string.to_graphemes("a") == ["a"]
}

pub fn to_graphemes_basic_test() {
  assert string.to_graphemes("abc") == ["a", "b", "c"]
}

pub fn to_graphemes_emoji_test() {
  assert string.to_graphemes("🌷🎁💩😜👍🏳️‍🌈") == ["🌷", "🎁", "💩", "😜", "👍", "🏳️‍🌈"]
}

pub fn to_graphemes_combining_test() {
  assert string.to_graphemes("Ĺo͂řȩm̅") == ["Ĺ", "o͂", "ř", "ȩ", "m̅"]
}

pub fn to_graphemes_korean_test() {
  assert string.to_graphemes("뎌쉐") == ["뎌", "쉐"]
}

pub fn to_graphemes_family_emoji_test() {
  assert string.to_graphemes("👨‍👩‍👦‍👦") == ["👨‍👩‍👦‍👦"]
}

pub fn to_graphemes_japanese_test() {
  assert string.to_graphemes("ごん゙に゙ぢば") == ["ご", "ん゙", "に゙", "ぢ", "ば"]
}

pub fn to_graphemes_katakana_test() {
  assert string.to_graphemes("パピプペポ") == ["パ", "ピ", "プ", "ペ", "ポ"]
}

pub fn to_graphemes_zalgo_test() {
  assert string.to_graphemes("Z͑ͫ̓ͪ̂ͫ̽͏̴̙̤̞͉͚̯̞̠͍A̴̵̜̰͔ͫ͗͢L̠ͨͧͩ͘G̴̻͈͍͔̹̑͗̎̅͛́Ǫ̵̹̻̝̳͂̌̌͘!͖̬̰̙̗̿̋ͥͥ̂ͣ̐́́͜͞") == ["Z͑ͫ̓ͪ̂ͫ̽͏̴̙̤̞͉͚̯̞̠͍", "A̴̵̜̰͔ͫ͗͢", "L̠ͨͧͩ͘", "G̴̻͈͍͔̹̑͗̎̅͛́", "Ǫ̵̹̻̝̳͂̌̌͘", "!͖̬̰̙̗̿̋ͥͥ̂ͣ̐́́͜͞"]
}

pub fn to_utf_codepoints_empty_test() {
  assert string.to_utf_codepoints("") == []
}

pub fn to_utf_codepoints_basic_test() {
  assert string.to_utf_codepoints("gleam")
    == {
      let assert #(Ok(g), Ok(l), Ok(e), Ok(a), Ok(m)) = #(
        string.utf_codepoint(103),
        string.utf_codepoint(108),
        string.utf_codepoint(101),
        string.utf_codepoint(97),
        string.utf_codepoint(109),
      )
      [g, l, e, a, m]
    }
}

pub fn to_utf_codepoints_rainbow_flag_test() {
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

  assert string.to_utf_codepoints("🏳️‍🌈") == expected
}

pub fn from_utf_codepoints_empty_test() {
  assert ""
    |> string.to_utf_codepoints
    |> string.from_utf_codepoints
    == ""
}

pub fn from_utf_codepoints_basic_test() {
  assert "gleam"
    |> string.to_utf_codepoints
    |> string.from_utf_codepoints
    == "gleam"
}

pub fn from_utf_codepoints_emoji_test() {
  assert "🏳️‍🌈"
    |> string.to_utf_codepoints
    |> string.from_utf_codepoints
    == "🏳️‍🌈"
}

pub fn from_utf_codepoints_abc_test() {
  assert string.from_utf_codepoints({
      let assert #(Ok(a), Ok(b), Ok(c)) = #(
        string.utf_codepoint(97),
        string.utf_codepoint(98),
        string.utf_codepoint(99),
      )
      [a, b, c]
    })
    == "abc"
}

pub fn utf_codepoint_negative_test() {
  // Less than the lower bound on valid codepoints
  let assert Error(_) = string.utf_codepoint(-1)
}

pub fn utf_codepoint_zero_test() {
  // The lower bound on valid codepoints
  let assert Ok(_) = string.utf_codepoint(0)
}

pub fn utf_codepoint_max_test() {
  // The upper bound for valid code points
  let assert Ok(_) = string.utf_codepoint(1_114_111)
}

pub fn utf_codepoint_above_max_test() {
  // Greater than the upper bound on valid codepoints
  let assert Error(_) = string.utf_codepoint(1_114_112)
}

pub fn utf_codepoint_nonchar_fffe_test() {
  // Non-characters U+FFFE and U+FFFF are valid codepoints.  See (#778).
  let assert Ok(_) = string.utf_codepoint(65_534)
}

pub fn utf_codepoint_nonchar_ffff_test() {
  let assert Ok(_) = string.utf_codepoint(65_535)
}

pub fn utf_codepoint_below_high_surrogate_test() {
  // One less than the lowest "High-surrogate code point"
  let assert Ok(_) = string.utf_codepoint(55_295)
}

pub fn utf_codepoint_high_surrogate_test() {
  // Lowest value of the "High-surrogate code point" (U+D800 to U+DBFF)
  let assert Error(_) = string.utf_codepoint(55_296)
}

pub fn utf_codepoint_low_surrogate_test() {
  // Highest value of the "Low-surrogate code point" (U+DC00 to U+DFFF)
  let assert Error(_) = string.utf_codepoint(57_343)
}

pub fn utf_codepoint_above_low_surrogate_test() {
  // One greater than the highest "Low-surrogate code point"
  let assert Ok(_) = string.utf_codepoint(57_344)
}

pub fn bit_array_utf_codepoint_test() {
  let assert Ok(snake) = string.utf_codepoint(128_013)
  let expected = <<"🐍":utf8>>
  assert <<snake:utf8_codepoint>> == expected
}

pub fn utf_codepoint_to_int_test() {
  assert string.utf_codepoint_to_int({
      let assert Ok(ordinal_value) = string.utf_codepoint(128_013)
      ordinal_value
    })
    == 128_013
}

pub fn to_option_empty_test() {
  assert string.to_option("") == None
}

pub fn to_option_non_empty_test() {
  assert string.to_option("ok") == Some("ok")
}

pub fn first_empty_test() {
  let assert Error(_) = string.first("")
}

pub fn first_basic_test() {
  assert string.first("gleam") == Ok("g")
}

pub fn first_emoji_test() {
  assert string.first("⭐️ Gleam") == Ok("⭐️")
}

pub fn first_single_test() {
  assert string.first("a") == Ok("a")
}

pub fn last_empty_test() {
  let assert Error(_) = string.last("")
}

pub fn last_basic_test() {
  assert string.last("gleam") == Ok("m")
}

pub fn last_with_space_test() {
  assert string.last("gleam ") == Ok(" ")
}

pub fn last_unicode_test() {
  assert string.last("եdelays") == Ok("s")
}

pub fn last_single_test() {
  assert string.last("a") == Ok("a")
}

pub fn capitalise_empty_test() {
  assert string.capitalise("") == ""
}

pub fn capitalise_lowercase_test() {
  assert string.capitalise("gleam") == "Gleam"
}

pub fn capitalise_uppercase_test() {
  assert string.capitalise("GLEAM") == "Gleam"
}

pub fn capitalise_with_spaces_test() {
  assert string.capitalise("g l e a m") == "G l e a m"
}

pub fn capitalise_leading_number_test() {
  assert string.capitalise("1GLEAM") == "1gleam"
}

pub fn capitalise_leading_underscore_test() {
  assert string.capitalise("_gLeAm1") == "_gleam1"
}

pub fn capitalise_leading_space_test() {
  assert string.capitalise(" gLeAm1") == " gleam1"
}

pub fn capitalise_unicode_test() {
  assert string.capitalise("る") == "る"
}

type InspectType(a, b) {
  InspectTypeZero
  InspectTypeOne(a)
  InspectTypeTwo(a, b)
}

pub fn inspect_0_test() {
  assert string.inspect(True) == "True"
}

pub fn inspect_4_test() {
  assert string.inspect(False) == "False"
}

pub fn inspect_8_test() {
  assert string.inspect([True, False]) == "[True, False]"
}

pub fn inspect_12_test() {
  assert string.inspect([False, False]) == "[False, False]"
}

pub fn inspect_16_test() {
  assert string.inspect([True, True]) == "[True, True]"
}

pub fn inspect_20_test() {
  assert string.inspect([Nil, Nil]) == "[Nil, Nil]"
}

pub fn inspect_24_test() {
  assert string.inspect(#(True, False)) == "#(True, False)"
}

pub fn inspect_28_test() {
  assert string.inspect(#(False, False)) == "#(False, False)"
}

pub fn inspect_32_test() {
  assert string.inspect(#(True, True)) == "#(True, True)"
}

pub fn inspect_36_test() {
  assert string.inspect(#(Nil, True)) == "#(Nil, True)"
}

pub fn inspect_40_test() {
  assert string.inspect(#(Nil, False)) == "#(Nil, False)"
}

pub fn inspect_44_test() {
  assert string.inspect(#(True, Nil)) == "#(True, Nil)"
}

pub fn inspect_48_test() {
  assert string.inspect(#(False, Nil)) == "#(False, Nil)"
}

pub fn inspect_52_test() {
  assert string.inspect(-1) == "-1"
}

pub fn inspect_56_test() {
  assert string.inspect(0) == "0"
}

pub fn inspect_60_test() {
  assert string.inspect(1) == "1"
}

pub fn inspect_64_test() {
  assert string.inspect([]) == "[]"
}

pub fn inspect_68_test() {
  assert string.inspect([1]) == "[1]"
}

pub fn inspect_72_test() {
  assert string.inspect([1, 2]) == "[1, 2]"
}

pub fn inspect_76_test() {
  assert string.inspect([[1], [1]]) == "[[1], [1]]"
}

pub fn inspect_80_test() {
  assert string.inspect(-1.5) == "-1.5"
}

pub fn inspect_84_test() {
  assert string.inspect(5.0e-26) == "5.0e-26"
}

pub fn inspect_88_test() {
  assert string.inspect(1.5) == "1.5"
}

pub fn inspect_92_test() {
  assert string.inspect(-5.0e-26) == "-5.0e-26"
}

pub fn inspect_96_test() {
  assert string.inspect([1.5]) == "[1.5]"
}

pub fn inspect_100_test() {
  assert string.inspect("") == "\"\""
}

pub fn inspect_104_test() {
  assert string.inspect("\\") == "\"\\\\\""
}

pub fn inspect_108_test() {
  assert string.inspect("\\\\") == "\"\\\\\\\\\""
}

pub fn inspect_112_test() {
  assert string.inspect("\\\\\\") == "\"\\\\\\\\\\\\\""
}

pub fn inspect_116_test() {
  assert string.inspect("\"") == "\"\\\"\""
}

pub fn inspect_120_test() {
  assert string.inspect("\"\"") == "\"\\\"\\\"\""
}

pub fn inspect_124_test() {
  assert string.inspect("\r") == "\"\\r\""
}

pub fn inspect_128_test() {
  assert string.inspect("\n") == "\"\\n\""
}

pub fn inspect_132_test() {
  assert string.inspect("\t") == "\"\\t\""
}

pub fn inspect_136_test() {
  assert string.inspect("\f") == "\"\\f\""
}

pub fn inspect_140_test() {
  assert string.inspect("\u{0008}") == "\"\\u{0008}\""
}

pub fn inspect_144_test() {
  assert string.inspect("\u{000B}") == "\"\\u{000B}\""
}

pub fn inspect_148_test() {
  assert string.inspect("\u{001B}") == "\"\\u{001B}\""
}

pub fn inspect_152_test() {
  assert string.inspect("\u{0015}") == "\"\\u{0015}\""
}

pub fn inspect_156_test() {
  assert string.inspect("\u{001F}") == "\"\\u{001F}\""
}

pub fn inspect_160_test() {
  assert string.inspect("\u{0020}") == "\" \""
}

pub fn inspect_164_test() {
  assert string.inspect("\u{007F}") == "\"\\u{007F}\""
}

pub fn inspect_168_test() {
  assert string.inspect("\u{009F}") == "\"\\u{009F}\""
}

pub fn inspect_172_test() {
  assert string.inspect("\u{00A0}") == "\"\u{00A0}\""
}

pub fn inspect_176_test() {
  assert string.inspect("\r\r") == "\"\\r\\r\""
}

pub fn inspect_180_test() {
  assert string.inspect("\n\n") == "\"\\n\\n\""
}

pub fn inspect_184_test() {
  assert string.inspect("\r\n") == "\"\\r\\n\""
}

pub fn inspect_188_test() {
  assert string.inspect("\n\r") == "\"\\n\\r\""
}

pub fn inspect_192_test() {
  assert string.inspect("\t\t") == "\"\\t\\t\""
}

pub fn inspect_196_test() {
  assert string.inspect("\t\n") == "\"\\t\\n\""
}

pub fn inspect_200_test() {
  assert string.inspect("\n\t") == "\"\\n\\t\""
}

pub fn inspect_204_test() {
  assert string.inspect("\t\r") == "\"\\t\\r\""
}

pub fn inspect_208_test() {
  assert string.inspect("\r\t") == "\"\\r\\t\""
}

pub fn inspect_212_test() {
  assert string.inspect("\t\f") == "\"\\t\\f\""
}

pub fn inspect_216_test() {
  assert string.inspect("\f\t") == "\"\\f\\t\""
}

pub fn inspect_220_test() {
  assert string.inspect("\t\u{0008}") == "\"\\t\\u{0008}\""
}

pub fn inspect_224_test() {
  assert string.inspect("\u{0008}\t") == "\"\\u{0008}\\t\""
}

pub fn inspect_228_test() {
  assert string.inspect("\t\u{000B}") == "\"\\t\\u{000B}\""
}

pub fn inspect_232_test() {
  assert string.inspect("\u{000B}\t") == "\"\\u{000B}\\t\""
}

pub fn inspect_236_test() {
  assert string.inspect("\t\u{001B}") == "\"\\t\\u{001B}\""
}

pub fn inspect_240_test() {
  assert string.inspect("\u{001B}\t") == "\"\\u{001B}\\t\""
}

pub fn inspect_244_test() {
  assert string.inspect("\\\n\\") == "\"\\\\\\n\\\\\""
}

pub fn inspect_248_test() {
  assert string.inspect("\\\"\\") == "\"\\\\\\\"\\\\\""
}

pub fn inspect_252_test() {
  assert string.inspect("\\\"\"\\") == "\"\\\\\\\"\\\"\\\\\""
}

pub fn inspect_256_test() {
  assert string.inspect("'") == "\"'\""
}

pub fn inspect_260_test() {
  assert string.inspect("''") == "\"''\""
}

pub fn inspect_264_test() {
  assert string.inspect("around-single-quotes'around-single-quotes")
    == "\"around-single-quotes'around-single-quotes\""
}

pub fn inspect_269_test() {
  assert string.inspect("'between-single-quotes'")
    == "\"'between-single-quotes'\""
}

pub fn inspect_274_test() {
  assert string.inspect("0") == "\"0\""
}

pub fn inspect_278_test() {
  assert string.inspect("1") == "\"1\""
}

pub fn inspect_282_test() {
  assert string.inspect("2") == "\"2\""
}

pub fn inspect_286_test() {
  assert string.inspect("Hello Joe!") == "\"Hello Joe!\""
}

pub fn inspect_290_test() {
  assert string.inspect("Hello \"Manuel\"!") == "\"Hello \\\"Manuel\\\"!\""
}

pub fn inspect_294_test() {
  assert string.inspect("👨‍👩‍👦‍👦 💜 Gleam") == "\"👨‍👩‍👦‍👦 💜 Gleam\""
}

pub fn inspect_298_test() {
  assert string.inspect("✨") == "\"✨\""
}

pub fn inspect_302_test() {
  assert string.inspect("🏳️‍⚧️") == "\"🏳️‍⚧️\""
}

pub fn inspect_306_test() {
  assert string.inspect("True") == "\"True\""
}

pub fn inspect_310_test() {
  assert string.inspect("False") == "\"False\""
}

pub fn inspect_314_test() {
  assert string.inspect("Nil") == "\"Nil\""
}

pub fn inspect_318_test() {
  assert string.inspect(["1"]) == "[\"1\"]"
}

pub fn inspect_322_test() {
  assert string.inspect(#()) == "#()"
}

pub fn inspect_326_test() {
  assert string.inspect(#(1)) == "#(1)"
}

pub fn inspect_330_test() {
  assert string.inspect(#("1")) == "#(\"1\")"
}

pub fn inspect_334_test() {
  assert string.inspect(#(1.5)) == "#(1.5)"
}

pub fn inspect_338_test() {
  assert string.inspect([#(1, 2, 3), #(1, 2, 3)]) == "[#(1, 2, 3), #(1, 2, 3)]"
}

pub fn inspect_342_test() {
  assert string.inspect(#([1, 2, 3], "🌈", "🏳️‍🌈", #(1, "1", True)))
    == "#([1, 2, 3], \"🌈\", \"🏳️‍🌈\", #(1, \"1\", True))"
}

pub fn inspect_347_test() {
  assert string.inspect(Nil) == "Nil"
}

pub fn inspect_351_test() {
  assert string.inspect(Ok(1)) == "Ok(1)"
}

pub fn inspect_355_test() {
  assert string.inspect(Ok(True)) == "Ok(True)"
}

pub fn inspect_359_test() {
  assert string.inspect(Ok(False)) == "Ok(False)"
}

pub fn inspect_363_test() {
  assert string.inspect(Ok(Nil)) == "Ok(Nil)"
}

pub fn inspect_367_test() {
  assert string.inspect(Error(2)) == "Error(2)"
}

pub fn inspect_371_test() {
  assert string.inspect(Error(True)) == "Error(True)"
}

pub fn inspect_375_test() {
  assert string.inspect(Error(False)) == "Error(False)"
}

pub fn inspect_379_test() {
  assert string.inspect(Error(Nil)) == "Error(Nil)"
}

pub fn inspect_383_test() {
  assert string.inspect(InspectTypeZero) == "InspectTypeZero"
}

pub fn inspect_387_test() {
  assert string.inspect(InspectTypeOne(1)) == "InspectTypeOne(1)"
}

pub fn inspect_391_test() {
  assert string.inspect(InspectTypeTwo(1, 2)) == "InspectTypeTwo(1, 2)"
}

pub fn inspect_395_test() {
  assert string.inspect(InspectTypeOne([1])) == "InspectTypeOne([1])"
}

pub fn inspect_399_test() {
  assert string.inspect(InspectTypeOne("1")) == "InspectTypeOne(\"1\")"
}

pub fn inspect_403_test() {
  assert string.inspect(InspectTypeOne(["1"])) == "InspectTypeOne([\"1\"])"
}

pub fn inspect_407_test() {
  assert string.inspect(InspectTypeOne(#([1], "a")))
    == "InspectTypeOne(#([1], \"a\"))"
}

pub fn inspect_412_test() {
  assert string.inspect(Ok) == "//fn(a) { ... }"
}

pub fn inspect_416_test() {
  assert string.inspect(Error) == "//fn(a) { ... }"
}

pub fn inspect_420_test() {
  assert string.inspect(fn() { Nil }) == "//fn() { ... }"
}

pub fn inspect_424_test() {
  assert string.inspect(fn(_) { Nil }) == "//fn(a) { ... }"
}

pub fn inspect_428_test() {
  assert string.inspect(fn(_, _) { Nil }) == "//fn(a, b) { ... }"
}

pub fn inspect_432_test() {
  assert string.inspect(fn(_, _) { Nil }) == "//fn(a, b) { ... }"
}

pub fn inspect_436_test() {
  assert string.inspect(fn(_: Int, _: String) -> Bool { False })
    == "//fn(a, b) { ... }"
}

pub fn inspect_441_test() {
  assert string.inspect(#(InspectTypeOne, InspectTypeTwo))
    == "#(//fn(a) { ... }, //fn(a, b) { ... })"
}

pub fn inspect_446_test() {
  assert string.inspect(InspectTypeOne(InspectTypeZero))
    == "InspectTypeOne(InspectTypeZero)"
}

pub fn inspect_451_test() {
  assert string.inspect(<<255, 2, 0>>) == "<<255, 2, 0>>"
}

pub fn inspect_charlist_test() {
  let list = [
    70, 97, 105, 108, 101, 100, 32, 116, 111, 32, 108, 111, 97, 100, 32, 78, 73,
    70, 32, 108, 105, 98, 114, 97, 114, 121, 58, 32, 39, 47, 114, 117, 110, 47,
    99, 117, 114, 114, 101, 110, 116, 45, 115, 121, 115, 116, 101, 109, 47, 115,
    119, 47, 115, 104, 97, 114, 101, 47, 110, 105, 120, 45, 108, 100, 47, 108,
    105, 98, 47, 108, 105, 98, 99, 114, 121, 112, 116, 111, 46, 115, 111, 46, 51,
  ]
  assert string.inspect(list)
    == "charlist.from_string(\"Failed to load NIF library: '/run/current-system/sw/share/nix-ld/lib/libcrypto.so.3\")"
}

@target(javascript)
pub fn target_inspect_tuple_test() {
  // Due to Erlang's internal representation, on Erlang this passes, instead:
  // string.inspect(#(InspectTypeZero, InspectTypeZero))
  // |> should.equal("InspectTypeZero(InspectTypeZero)")
  assert string.inspect(#(InspectTypeZero, InspectTypeZero))
    == "#(InspectTypeZero, InspectTypeZero)"
}

@target(javascript)
pub fn target_inspect_float_negative_test() {
  // Due to JavaScript's `Number` type `Float`s without digits return as
  // `Int`s.
  assert string.inspect(-1.0) == "-1"
}

@target(javascript)
pub fn target_inspect_float_zero_test() {
  assert string.inspect(0.0) == "0"
}

@target(javascript)
pub fn target_inspect_float_positive_test() {
  assert string.inspect(1.0) == "1"
}

@target(javascript)
pub fn target_inspect_float_list_test() {
  assert string.inspect([1.0]) == "[1]"
}

@target(javascript)
pub fn target_inspect_float_tuple_test() {
  assert string.inspect(#(1.0)) == "#(1)"
}

@target(javascript)
pub fn target_inspect_bit_array_test() {
  // Unlike on Erlang, on JavaScript `BitArray` and `String` do have a
  // different runtime representation.
  assert string.inspect(<<"abc":utf8>>) == "<<97, 98, 99>>"
}

@target(erlang)
@external(erlang, "erlang", "self")
fn create_erlang_pid() -> String

@target(erlang)
@external(erlang, "erlang", "make_ref")
fn create_erlang_reference() -> String

@target(erlang)
pub fn target_inspect_tuple_test() {
  // Erlang's internal representation does not allow a correct
  // differentiation at runtime and thus this does not pass:
  // string.inspect(#(InspectTypeZero, InspectTypeZero))
  // |> should.equal("#(InspectTypeZero, InspectTypeZero)")
  assert string.inspect(#(InspectTypeZero, InspectTypeZero))
    == "InspectTypeZero(InspectTypeZero)"
}

@target(erlang)
pub fn target_inspect_float_negative_test() {
  // Unlike JavaScript, Erlang correctly differentiates between `1` and `1.0`
  // at runtime.
  assert string.inspect(-1.0) == "-1.0"
}

@target(erlang)
pub fn target_inspect_float_zero_test() {
  assert string.inspect(0.0) == "0.0"
}

@target(erlang)
pub fn target_inspect_float_positive_test() {
  assert string.inspect(1.0) == "1.0"
}

@target(erlang)
pub fn target_inspect_float_list_test() {
  assert string.inspect([1.0]) == "[1.0]"
}

@target(erlang)
pub fn target_inspect_float_tuple_test() {
  assert string.inspect(#(1.0)) == "#(1.0)"
}

@target(erlang)
pub fn target_inspect_pid_test() {
  // Looks like `//erl(<0.83.0>)`.
  assert looks_like_pid(string.inspect(create_erlang_pid()))
}

@target(erlang)
pub fn target_inspect_ref_test() {
  // Looks like: `//erl(#Ref<0.1809744150.4035444737.100468>)`.
  assert looks_like_ref(string.inspect(create_erlang_reference()))
}

@target(erlang)
pub fn target_inspect_bit_array_test() {
  // On Erlang the representation between `String` and `BitArray` is
  // indistinguishable at runtime.
  assert string.inspect(<<"abc":utf8>>) == "\"abc\""
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
import gleam/dynamic.{type Dynamic}

@target(erlang)
pub fn inspect_erlang_atom_one_two_test() {
  assert string.inspect(string_to_erlang_atom("one_two")) == "OneTwo"
}

@target(erlang)
pub fn inspect_erlang_atom_one1_two_test() {
  assert string.inspect(string_to_erlang_atom("one1_two")) == "One1Two"
}

@target(erlang)
pub fn inspect_erlang_atom_one1two_test() {
  assert string.inspect(string_to_erlang_atom("one1two")) == "One1two"
}

@target(erlang)
pub fn inspect_erlang_atom_with_a_leading_underscore_is_invalid_in_gleam_test() {
  assert string.inspect(string_to_erlang_atom("_ok")) == "atom.create(\"_ok\")"
}

@target(erlang)
pub fn inspect_erlang_atom_with_a_trailing_underscore_is_invalid_in_gleam_test() {
  assert string.inspect(string_to_erlang_atom("ok_")) == "atom.create(\"ok_\")"
}

@target(erlang)
pub fn inspect_erlang_atom_with_a_double_underscore_is_invalid_in_gleam_test() {
  assert string.inspect(string_to_erlang_atom("ok__ok"))
    == "atom.create(\"ok__ok\")"
}

@target(erlang)
pub fn inspect_erlang_atom_with_white_spaces_is_invalid_in_gleam_test() {
  assert string.inspect(string_to_erlang_atom("ok ok"))
    == "atom.create(\"ok ok\")"
}

@target(erlang)
pub fn inspect_erlang_atom_that_is_an_empty_string_is_invalid_in_gleam_test() {
  // An empty string based atom is invalid in gleam
  assert string.inspect(string_to_erlang_atom("")) == "atom.create(\"\")"
}

@target(erlang)
pub fn inspect_erlang_atom_with_uppercases_invalid_in_gleam_test() {
  assert string.inspect(string_to_erlang_atom("Upper"))
    == "atom.create(\"Upper\")"
}

@target(erlang)
pub fn inspect_erlang_atom_tag_tuple_test() {
  assert string.inspect(#(string_to_erlang_atom("DOWN"), 1, 2))
    == "#(atom.create(\"DOWN\"), 1, 2)"
}

@target(erlang)
pub fn inspect_erlang_atom_with_leading_digit_1_ok_test() {
  assert string.inspect(string_to_erlang_atom("1_ok"))
    == "atom.create(\"1_ok\")"
}

@target(erlang)
pub fn inspect_erlang_atom_with_leading_digit_1_ok_camelcase_test() {
  assert string.inspect(string_to_erlang_atom("1Ok")) == "atom.create(\"1Ok\")"
}

@target(erlang)
pub fn inspect_fifteen_bit_int_test() {
  assert string.inspect(<<2, 3:size(7)>>) == "<<2, 3:size(7)>>"
}

@target(javascript)
@external(javascript, "../gleam_stdlib_test_ffi.mjs", "circular_reference")
fn circular_reference() -> Dynamic

@target(javascript)
pub fn inspect_circular_reference_test() {
  assert string.inspect(circular_reference())
    == "#(1, 2, 3, //js(circular reference))"
}

@target(javascript)
@external(javascript, "../gleam_stdlib_test_ffi.mjs", "singleton_object")
fn singleton_object() -> Dynamic

@target(javascript)
pub fn inspect_singleton_test() {
  assert string.inspect(#(singleton_object(), singleton_object()))
    == "#(//js({ \"a\": 1 }), //js({ \"a\": 1 }))"
}

@target(javascript)
@external(javascript, "../gleam_stdlib_test_ffi.mjs", "js_error")
fn js_error() -> Dynamic

@target(javascript)
pub fn inspect_js_error_test() {
  assert string.inspect(js_error()) == "//js(SomeError: Oh no!)"
}

pub fn byte_size_empty_test() {
  let assert 0 = string.byte_size("")
}

pub fn byte_size_one_test() {
  let assert 1 = string.byte_size("a")
}

pub fn byte_size_two_test() {
  let assert 2 = string.byte_size("ab")
}

pub fn byte_size_three_test() {
  let assert 3 = string.byte_size("abc")
}

pub fn byte_size_unicode_a_test() {
  // Unicode graphemes. These will be multiple bytes.
  let assert 1 = string.byte_size("a")
}

pub fn byte_size_unicode_umlaut_test() {
  let assert 2 = string.byte_size("ä")
}

pub fn byte_size_unicode_emoji_test() {
  let assert 4 = string.byte_size("👩")
}

pub fn byte_size_unicode_skin_tone_test() {
  let assert 8 = string.byte_size("👩🏾")
}

pub fn byte_size_unicode_complex_test() {
  let assert 15 = string.byte_size("👩🏾‍🦰")
}

pub fn inspect_map_test() {
  assert string.inspect(dict.from_list([#("a", 1), #("b", 2)]))
    == "dict.from_list([#(\"a\", 1), #(\"b\", 2)])"
}

pub fn trim_prefix_test() {
  assert string.trim_prefix("gleamlucy", "gleam") == "lucy"

  assert string.trim_prefix("lucygleam", "gleam") == "lucygleam"

  assert string.trim_prefix("lucygleam", "") == "lucygleam"

  assert string.trim_prefix("", "gleam") == ""

  assert string.trim_prefix("gleam", "gleam") == ""

  assert string.trim_prefix("🦑lucy", "🦑") == "lucy"

  assert string.trim_prefix("🦑", "🦑") == ""
}

pub fn trim_suffix_test() {
  assert string.trim_suffix("lucygleam", "gleam") == "lucy"

  assert string.trim_suffix("gleamlucy", "gleam") == "gleamlucy"

  assert string.trim_suffix("lucygleam", "") == "lucygleam"

  assert string.trim_suffix("", "gleam") == ""

  assert string.trim_suffix("gleam", "gleam") == ""

  assert string.trim_suffix("lucy🦑", "🦑") == "lucy"

  assert string.trim_suffix("🦑", "🦑") == ""
}

pub fn strip_prefix_test() {
  assert string.strip_prefix("gleamlucy", "gleam") == Ok("lucy")

  assert string.strip_prefix("lucygleam", "gleam") == Error(Nil)

  assert string.strip_prefix("lucygleam", "") == Ok("lucygleam")

  assert string.strip_prefix("", "gleam") == Error(Nil)

  assert string.strip_prefix("gleam", "gleam") == Ok("")

  assert string.strip_prefix("🦑lucy", "🦑") == Ok("lucy")

  assert string.strip_prefix("🦑", "🦑") == Ok("")
}

pub fn strip_suffix_test() {
  assert string.strip_suffix("lucygleam", "gleam") == Ok("lucy")

  assert string.strip_suffix("gleamlucy", "gleam") == Error(Nil)

  assert string.strip_suffix("lucygleam", "") == Ok("lucygleam")

  assert string.strip_suffix("", "gleam") == Error(Nil)

  assert string.strip_suffix("gleam", "gleam") == Ok("")

  assert string.strip_suffix("lucy🦑", "🦑") == Ok("lucy")

  assert string.strip_suffix("🦑", "🦑") == Ok("")
}
