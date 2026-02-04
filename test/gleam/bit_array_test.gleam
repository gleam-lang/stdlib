import gleam/bit_array
import gleam/order
import gleam/result
import gleam/string

pub fn bit_size_empty_test() {
  assert bit_array.bit_size(<<>>) == 0
}

pub fn bit_size_one_byte_test() {
  assert bit_array.bit_size(<<0>>) == 8
}

pub fn bit_size_32_bits_test() {
  assert bit_array.bit_size(<<-1:32>>) == 32
}

pub fn bit_size_negative_size_test() {
  assert bit_array.bit_size(<<0:-8>>) == 0
}

pub fn bit_size_one_bit_test() {
  assert bit_array.bit_size(<<0:1>>) == 1
}

pub fn bit_size_three_bits_test() {
  assert bit_array.bit_size(<<7:3>>) == 3
}

pub fn bit_size_190_bits_test() {
  assert bit_array.bit_size(<<-1:190>>) == 190
}

pub fn bit_size_negative_one_test() {
  assert bit_array.bit_size(<<0:-1>>) == 0
}

pub fn byte_size_empty_test() {
  assert bit_array.byte_size(<<>>) == 0
}

pub fn byte_size_five_bytes_test() {
  assert bit_array.byte_size(<<0, 1, 2, 3, 4>>) == 5
}

pub fn byte_size_unaligned_test() {
  assert bit_array.byte_size(<<1, 2, 3:6>>) == 3
}

pub fn pad_to_bytes_empty_test() {
  assert bit_array.pad_to_bytes(<<>>) == <<>>
}

pub fn pad_to_bytes_one_byte_test() {
  assert bit_array.pad_to_bytes(<<0xAB>>) == <<0xAB>>
}

pub fn pad_to_bytes_two_bytes_test() {
  assert bit_array.pad_to_bytes(<<0xAB, 0x12>>) == <<0xAB, 0x12>>
}

pub fn pad_to_bytes_one_bit_test() {
  assert bit_array.pad_to_bytes(<<1:1>>) == <<0x80>>
}

pub fn pad_to_bytes_seven_bits_test() {
  assert bit_array.pad_to_bytes(<<-1:7>>) == <<0xFE>>
}

pub fn pad_to_bytes_unaligned_test() {
  assert bit_array.pad_to_bytes(<<0xAB, 0x12, 3:3>>) == <<0xAB, 0x12, 0x60>>
}

pub fn pad_to_bytes_slice_test() {
  let assert <<a:bits-12, _:4>> = <<0xAB, 0xFF>>
  assert bit_array.pad_to_bytes(a) == <<0xAB, 0xF0>>
}

pub fn not_equal_test() {
  assert bit_array.from_string("test") != bit_array.from_string("asdf")
}

pub fn append_strings_test() {
  assert bit_array.append(
      bit_array.from_string("Test"),
      bit_array.from_string(" Me"),
    )
    == bit_array.from_string("Test Me")
}

pub fn append_empty_test() {
  assert bit_array.append(<<1, 2>>, <<>>) == <<1, 2>>
}

pub fn append_bytes_test() {
  assert bit_array.append(<<1, 2>>, <<3, 4>>) == <<1, 2, 3, 4>>
}

pub fn append_unaligned_test() {
  assert bit_array.append(<<1, 2:4>>, <<3>>) == <<1, 2:4, 3>>
}

pub fn concat_single_test() {
  assert bit_array.concat([<<1, 2>>]) == <<1, 2>>
}

pub fn concat_multiple_test() {
  assert bit_array.concat([<<1, 2>>, <<3>>, <<4>>]) == <<1, 2, 3, 4>>
}

pub fn concat_unaligned_test() {
  assert bit_array.concat([<<-1:32>>, <<0:1>>, <<0:0>>])
    == <<255, 255, 255, 255, 0:1>>
}

pub fn concat_complex_test() {
  assert bit_array.concat([<<-20:6, 2>>, <<3:4>>, <<7:3>>, <<-1:64>>])
    == <<176, 8, 255, 255, 255, 255, 255, 255, 255, 255, 31:size(5)>>
}

pub fn concat_unaligned_2_test() {
  assert bit_array.concat([<<1, 2:4>>, <<3>>]) == <<1, 2:4, 3>>
}

pub fn concat_unaligned_3_test() {
  assert bit_array.concat([<<-1:32>>, <<0:1>>, <<0:0>>])
    == <<255, 255, 255, 255, 0:1>>
}

pub fn concat_complex_2_test() {
  assert bit_array.concat([<<-20:6, 2>>, <<3:4>>, <<7:3>>, <<-1:64>>])
    == <<176, 8, 255, 255, 255, 255, 255, 255, 255, 255, 31:size(5)>>
}

pub fn slice_full_test() {
  assert bit_array.slice(<<"hello":utf8>>, 0, 5) == Ok(<<"hello":utf8>>)
}

pub fn slice_empty_test() {
  assert bit_array.slice(<<"hello":utf8>>, 0, 0) == Ok(<<"":utf8>>)
}

pub fn slice_middle_test() {
  assert bit_array.slice(<<"hello":utf8>>, 2, 2) == Ok(<<"ll":utf8>>)
}

pub fn slice_negative_length_test() {
  assert bit_array.slice(<<"hello":utf8>>, 5, -2) == Ok(<<"lo":utf8>>)
}

pub fn slice_empty_string_test() {
  assert bit_array.slice(<<"":utf8>>, 0, 0) == Ok(<<"":utf8>>)
}

pub fn slice_out_of_bounds_test() {
  assert bit_array.slice(<<"hello":utf8>>, 6, 0) == Error(Nil)
}

pub fn slice_invalid_negative_test() {
  assert bit_array.slice(<<"hello":utf8>>, 1, -2) == Error(Nil)
}

pub fn slice_negative_start_test() {
  assert bit_array.slice(bit_array.from_string("hello"), -1, 1) == Error(Nil)
}

pub fn slice_length_too_long_test() {
  assert bit_array.slice(bit_array.from_string("hello"), 1, 6) == Error(Nil)
}

pub fn slice_chained_test() {
  assert bit_array.from_string("ab")
    |> bit_array.slice(1, 1)
    |> result.try(bit_array.slice(_, 0, 1))
    == Ok(<<"b":utf8>>)
}

pub fn slice_unaligned_error_test() {
  assert bit_array.slice(<<0, 1, 2:7>>, 0, 3) == Error(Nil)
}

pub fn slice_large_error_test() {
  assert bit_array.slice(
      <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
      8,
      12,
    )
    == Error(Nil)
}

pub fn to_string_empty_test() {
  assert bit_array.to_string(<<>>) == Ok("")
}

pub fn to_string_empty_utf8_test() {
  assert bit_array.to_string(<<"":utf8>>) == Ok("")
}

pub fn to_string_hello_test() {
  assert bit_array.to_string(<<"Hello":utf8>>) == Ok("Hello")
}

pub fn to_string_unicode_test() {
  assert bit_array.to_string(<<"ø":utf8>>) == Ok("ø")
}

pub fn to_string_invalid_test() {
  assert bit_array.to_string(<<255>>) == Error(Nil)
}

pub fn to_string_unaligned_error_test() {
  assert bit_array.to_string(<<"ø":utf8, 2:4>>) == Error(Nil)
}

pub fn to_string_offset_test() {
  let assert <<_:3, x:bits>> = <<0:3, "ø":utf8>>
  assert bit_array.to_string(x) == Ok("ø")
}

pub fn is_utf8_empty_test() {
  assert bit_array.is_utf8(<<>>)
}

pub fn is_utf8_empty_string_test() {
  assert bit_array.is_utf8(<<"":utf8>>)
}

pub fn is_utf8_hello_test() {
  assert bit_array.is_utf8(<<"Hello":utf8>>)
}

pub fn is_utf8_unicode_test() {
  assert bit_array.is_utf8(<<"ø":utf8>>)
}

pub fn is_utf8_invalid_test() {
  assert !bit_array.is_utf8(<<255>>)
}

pub fn base64_encode_padded_test() {
  assert bit_array.base64_encode(<<255, 127, 254, 252>>, True) == "/3/+/A=="
}

pub fn base64_encode_padded_5_bytes_test() {
  assert bit_array.base64_encode(<<255, 127, 254, 252, 100>>, True)
    == "/3/+/GQ="
}

pub fn base64_encode_unpadded_test() {
  assert bit_array.base64_encode(<<255, 127, 254, 252>>, False) == "/3/+/A"
}

pub fn base64_encode_zeros_test() {
  assert bit_array.base64_encode(<<0, 0, 0>>, True) == "AAAA"
}

pub fn base64_encode_empty_test() {
  assert bit_array.base64_encode(<<>>, True) == ""
}

pub fn base64_encode_large_test() {
  assert string.repeat("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 1024 * 32)
    |> bit_array.from_string
    |> bit_array.base64_encode(True)
    == string.repeat("QUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFB", 1024 * 32)
}

pub fn base64_encode_unaligned_test() {
  assert bit_array.base64_encode(<<-1:7>>, True) == "/g=="
}

pub fn base64_encode_unaligned_2_test() {
  assert bit_array.base64_encode(<<0xFA, 5:3>>, True) == "+qA="
}

pub fn base64_encode_unaligned_3_test() {
  assert bit_array.base64_encode(<<0xFA, 0xBC, 0x6D, 1:1>>, True) == "+rxtgA=="
}

pub fn base64_decode_padded_test() {
  assert bit_array.base64_decode("/3/+/A==") == Ok(<<255, 127, 254, 252>>)
}

pub fn base64_decode_unpadded_test() {
  assert bit_array.base64_decode("/3/+/A") == Ok(<<255, 127, 254, 252>>)
}

pub fn base64_decode_zeros_test() {
  assert bit_array.base64_decode("AAAA") == Ok(<<0, 0, 0>>)
}

pub fn base64_decode_empty_test() {
  assert bit_array.base64_decode("") == Ok(<<>>)
}

pub fn base64_decode_invalid_test() {
  assert bit_array.base64_decode(")!") == Error(Nil)
}

pub fn base64_decode_invalid_padding_test() {
  assert bit_array.base64_decode("=AAA") == Error(Nil)
}

pub fn base64_url_encode_padded_test() {
  assert bit_array.base64_url_encode(<<255, 127, 254, 252>>, True) == "_3_-_A=="
}

pub fn base64_url_encode_unpadded_test() {
  assert bit_array.base64_url_encode(<<255, 127, 254, 252>>, False) == "_3_-_A"
}

pub fn base64_url_encode_zeros_test() {
  assert bit_array.base64_url_encode(<<0, 0, 0>>, True) == "AAAA"
}

pub fn base64_url_encode_empty_test() {
  assert bit_array.base64_url_encode(<<>>, True) == ""
}

pub fn base64_url_decode_padded_test() {
  assert bit_array.base64_url_decode("_3_-_A==") == Ok(<<255, 127, 254, 252>>)
}

pub fn base64_url_decode_unpadded_test() {
  assert bit_array.base64_url_decode("_3_-_A") == Ok(<<255, 127, 254, 252>>)
}

pub fn base64_url_decode_zeros_test() {
  assert bit_array.base64_url_decode("AAAA") == Ok(<<0, 0, 0>>)
}

pub fn base64_url_decode_empty_test() {
  assert bit_array.base64_url_decode("") == Ok(<<>>)
}

pub fn base64_url_decode_invalid_test() {
  assert bit_array.base64_url_decode(")!") == Error(Nil)
}

pub fn base64_decode_crash_regression_1_test() {
  assert bit_array.base64_decode("aGktdGhlcmU.uWUWvrAleKQ2jsWcU97H-RPJ5qRRcE_s")
    == Error(Nil)
}

pub fn base16_encode_empty_test() {
  assert bit_array.base16_encode(<<"":utf8>>) == ""
}

pub fn base16_encode_f_test() {
  assert bit_array.base16_encode(<<"f":utf8>>) == "66"
}

pub fn base16_encode_fo_test() {
  assert bit_array.base16_encode(<<"fo":utf8>>) == "666F"
}

pub fn base16_encode_foo_test() {
  assert bit_array.base16_encode(<<"foo":utf8>>) == "666F6F"
}

pub fn base16_encode_foob_test() {
  assert bit_array.base16_encode(<<"foob":utf8>>) == "666F6F62"
}

pub fn base16_encode_fooba_test() {
  assert bit_array.base16_encode(<<"fooba":utf8>>) == "666F6F6261"
}

pub fn base16_encode_foobar_test() {
  assert bit_array.base16_encode(<<"foobar":utf8>>) == "666F6F626172"
}

pub fn base16_encode_binary_test() {
  assert bit_array.base16_encode(<<161, 178, 195, 212, 229, 246, 120, 145>>)
    == "A1B2C3D4E5F67891"
}

pub fn base16_encode_unaligned_7_test() {
  assert bit_array.base16_encode(<<-1:7>>) == "FE"
}

pub fn base16_encode_unaligned_3_test() {
  assert bit_array.base16_encode(<<0xFA, 5:3>>) == "FAA0"
}

pub fn base16_encode_unaligned_4_test() {
  assert bit_array.base16_encode(<<0xFA, 5:4>>) == "FA50"
}

pub fn base16_encode_unaligned_1_test() {
  assert bit_array.base16_encode(<<0xFA, 0xBC, 0x6D, 1:1>>) == "FABC6D80"
}

pub fn base16_decode_empty_test() {
  assert bit_array.base16_decode("") == Ok(<<>>)
}

pub fn base16_decode_f_test() {
  assert bit_array.base16_decode("66") == Ok(<<"f":utf8>>)
}

pub fn base16_decode_fo_test() {
  assert bit_array.base16_decode("666F") == Ok(<<"fo":utf8>>)
}

pub fn base16_decode_foo_test() {
  assert bit_array.base16_decode("666F6F") == Ok(<<"foo":utf8>>)
}

pub fn base16_decode_foob_test() {
  assert bit_array.base16_decode("666F6F62") == Ok(<<"foob":utf8>>)
}

pub fn base16_decode_fooba_test() {
  assert bit_array.base16_decode("666F6F6261") == Ok(<<"fooba":utf8>>)
}

pub fn base16_decode_foobar_test() {
  assert bit_array.base16_decode("666F6F626172") == Ok(<<"foobar":utf8>>)
}

pub fn base16_decode_binary_test() {
  assert bit_array.base16_decode("A1B2C3D4E5F67891")
    == Ok(<<161, 178, 195, 212, 229, 246, 120, 145>>)
}

pub fn base16_decode_invalid_test() {
  // Not a hex string
  assert bit_array.base16_decode("?") == Error(Nil)
}

pub fn base16_decode_lowercase_test() {
  // Lowercase hex
  assert bit_array.base16_decode("a1b2c3d4e5f67891")
    == Ok(<<161, 178, 195, 212, 229, 246, 120, 145>>)
}

pub fn inspect_empty_test() {
  assert bit_array.inspect(<<>>) == "<<>>"
}

pub fn inspect_single_byte_test() {
  assert bit_array.inspect(<<80>>) == "<<80>>"
}

pub fn inspect_multiple_bytes_test() {
  assert bit_array.inspect(<<0, 20, 0x20, 255>>) == "<<0, 20, 32, 255>>"
}

pub fn inspect_unaligned_test() {
  assert bit_array.inspect(<<4:5>>) == "<<4:size(5)>>"
}

pub fn inspect_unaligned_2_test() {
  assert bit_array.inspect(<<100, 5:3>>) == "<<100, 5:size(3)>>"
}

pub fn inspect_unaligned_3_test() {
  assert bit_array.inspect(<<5:3, 11:4, 1:2>>) == "<<182, 1:size(1)>>"
}

pub fn compare_equal_test() {
  assert bit_array.compare(<<4:5>>, <<4:5>>) == order.Eq
}

pub fn compare_gt_test() {
  assert bit_array.compare(<<4:5, 3:3>>, <<4:5, 2:3>>) == order.Gt
}

pub fn compare_lt_test() {
  assert bit_array.compare(<<4:5, 3:3>>, <<4:5, 4:3>>) == order.Lt
}

pub fn compare_equal_with_zero_test() {
  assert bit_array.compare(<<4:5, 3:3, 0:0>>, <<4:5, 3:3, 0:0>>) == order.Eq
}

pub fn compare_different_sizes_gt_test() {
  assert bit_array.compare(<<0:2, 3:4, 0:0>>, <<0:2, 3:3, 0:0>>) == order.Gt
}

pub fn compare_different_values_lt_test() {
  // first is: <<33, 1:size(1)>>
  // second is: <<35>>
  assert bit_array.compare(<<4:5, 3:4, 0:0>>, <<4:5, 3:3, 0:0>>) == order.Lt
}

pub fn compare_lt_5_bits_test() {
  assert bit_array.compare(<<3:5>>, <<4:5>>) == order.Lt
}

pub fn compare_lt_7_bits_test() {
  assert bit_array.compare(<<3:7>>, <<4:7>>) == order.Lt
}

pub fn compare_gt_5_bits_test() {
  assert bit_array.compare(<<5:5>>, <<4:5>>) == order.Gt
}

pub fn compare_longer_gt_test() {
  assert bit_array.compare(<<4:8>>, <<4:5>>) == order.Gt
}

pub fn compare_shorter_lt_test() {
  assert bit_array.compare(<<4:5>>, <<4:8>>) == order.Lt
}

pub fn compare_zero_shorter_lt_test() {
  assert bit_array.compare(<<0:5>>, <<0:8>>) == order.Lt
}

pub fn compare_zero_equal_test() {
  assert bit_array.compare(<<0:5>>, <<0:5>>) == order.Eq
}

pub fn compare_zero_longer_gt_test() {
  assert bit_array.compare(<<0:2>>, <<0:1>>) == order.Gt
}

pub fn starts_with_empty_empty_test() {
  assert bit_array.starts_with(<<>>, <<>>)
}

pub fn starts_with_nonempty_empty_test() {
  assert bit_array.starts_with(<<0>>, <<>>)
}

pub fn starts_with_empty_nonempty_test() {
  assert !bit_array.starts_with(<<>>, <<0>>)
}

pub fn starts_with_single_byte_test() {
  assert bit_array.starts_with(<<0, 1, 2>>, <<0>>)
}

pub fn starts_with_two_bytes_test() {
  assert bit_array.starts_with(<<0, 1, 2>>, <<0, 1>>)
}

pub fn starts_with_full_match_test() {
  assert bit_array.starts_with(<<0, 1, 2>>, <<0, 1, 2>>)
}

pub fn starts_with_longer_prefix_test() {
  assert !bit_array.starts_with(<<0, 1, 2>>, <<0, 1, 2, 3>>)
}

pub fn starts_with_wrong_prefix_test() {
  assert !bit_array.starts_with(<<0, 1, 2>>, <<1>>)
}

pub fn starts_with_one_bit_test() {
  assert bit_array.starts_with(<<1:1>>, <<1:1>>)
}

pub fn starts_with_one_bit_empty_test() {
  assert bit_array.starts_with(<<1:1>>, <<>>)
}

pub fn starts_with_one_bit_longer_test() {
  assert !bit_array.starts_with(<<1:1>>, <<1:2>>)
}

pub fn starts_with_long_test() {
  assert bit_array.starts_with(<<-1:127>>, <<-1:33>>)
}

pub fn starts_with_long_longer_test() {
  assert !bit_array.starts_with(<<-1:127>>, <<-1:128>>)
}

pub fn starts_with_different_values_test() {
  assert !bit_array.starts_with(<<0:127>>, <<1:127>>)
}

pub fn starts_with_partial_byte_match_test() {
  assert bit_array.starts_with(<<0xFF, 0x81>>, <<0xFF, 1:1>>)
}

pub fn starts_with_partial_byte_no_match_test() {
  assert !bit_array.starts_with(<<0xFF, 0x81>>, <<0xFF, 0:1>>)
}
