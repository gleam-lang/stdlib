import gleam/int
import gleam/list
import gleam/order

pub fn absolute_value_positive_test() {
  assert int.absolute_value(123) == 123
}

pub fn absolute_value_negative_test() {
  assert int.absolute_value(-123) == 123
}

pub fn clamp_within_range_test() {
  assert int.clamp(40, min: 30, max: 50) == 40
}

pub fn clamp_below_min_test() {
  assert int.clamp(20, min: 30, max: 50) == 30
}

pub fn clamp_above_max_test() {
  assert int.clamp(60, min: 30, max: 50) == 50
}

pub fn clamp_inverted_range_above_test() {
  assert int.clamp(100, min: 50, max: 30) == 50
}

pub fn clamp_inverted_range_within_test() {
  assert int.clamp(40, min: 50, max: 30) == 40
}

pub fn to_string_positive_test() {
  assert int.to_string(123) == "123"
}

pub fn to_string_negative_test() {
  assert int.to_string(-123) == "-123"
}

pub fn to_string_positive_2_test() {
  assert int.to_string(123) == "123"
}

pub fn parse_positive_test() {
  assert int.parse("123") == Ok(123)
}

pub fn parse_negative_test() {
  assert int.parse("-123") == Ok(-123)
}

pub fn parse_leading_zero_test() {
  assert int.parse("0123") == Ok(123)
}

pub fn parse_empty_test() {
  assert int.parse("") == Error(Nil)
}

pub fn parse_invalid_test() {
  assert int.parse("what") == Error(Nil)
}

pub fn parse_float_test() {
  assert int.parse("1.23") == Error(Nil)
}

pub fn base_parse_hex_test() {
  assert int.base_parse("100", 16) == Ok(256)
}

pub fn base_parse_hex_negative_test() {
  assert int.base_parse("-100", 16) == Ok(-256)
}

pub fn base_parse_base_1_test() {
  assert int.base_parse("100", 1) == Error(Nil)
}

pub fn base_parse_base_37_test() {
  assert int.base_parse("100", 37) == Error(Nil)
}

pub fn base_parse_invalid_hex_test() {
  assert int.base_parse("AG", 16) == Error(Nil)
}

pub fn to_base_string_hex_test() {
  assert int.to_base_string(100, 16) == Ok("64")
}

pub fn to_base_string_hex_negative_test() {
  assert int.to_base_string(-100, 16) == Ok("-64")
}

pub fn to_base_string_base_1_test() {
  assert int.to_base_string(100, 1) == Error(Nil)
}

pub fn to_base_string_base_37_test() {
  assert int.to_base_string(100, 37) == Error(Nil)
}

pub fn to_base2_positive_test() {
  assert int.to_base2(100) == "1100100"
}

pub fn to_base2_negative_test() {
  assert int.to_base2(-100) == "-1100100"
}

pub fn to_base8_positive_test() {
  assert int.to_base8(100) == "144"
}

pub fn to_base8_negative_test() {
  assert int.to_base8(-100) == "-144"
}

pub fn to_base16_positive_test() {
  assert int.to_base16(100) == "64"
}

pub fn to_base16_negative_test() {
  assert int.to_base16(-100) == "-64"
}

pub fn to_base16_large_test() {
  assert int.to_base16(43_981) == "ABCD"
}

pub fn to_base16_large_negative_test() {
  assert int.to_base16(-43_981) == "-ABCD"
}

pub fn to_base36_positive_test() {
  assert int.to_base36(100) == "2S"
}

pub fn to_base36_negative_test() {
  assert int.to_base36(-100) == "-2S"
}

pub fn to_float_one_test() {
  assert int.to_float(1) == 1.0
}

pub fn to_float_five_test() {
  assert int.to_float(5) == 5.0
}

pub fn to_float_zero_test() {
  assert int.to_float(0) == 0.0
}

pub fn to_float_negative_test() {
  assert int.to_float(-5) == -5.0
}

pub fn compare_equal_zero_test() {
  assert int.compare(0, 0) == order.Eq
}

pub fn compare_equal_one_test() {
  assert int.compare(1, 1) == order.Eq
}

pub fn compare_lt_test() {
  assert int.compare(0, 1) == order.Lt
}

pub fn compare_negative_lt_test() {
  assert int.compare(-2, -1) == order.Lt
}

pub fn compare_gt_test() {
  assert int.compare(2, 1) == order.Gt
}

pub fn compare_negative_gt_test() {
  assert int.compare(-1, -2) == order.Gt
}

pub fn min_equal_test() {
  assert int.min(0, 0) == 0
}

pub fn min_first_smaller_test() {
  assert int.min(0, 1) == 0
}

pub fn min_second_smaller_test() {
  assert int.min(1, 0) == 0
}

pub fn min_negative_smaller_test() {
  assert int.min(-1, 2) == -1
}

pub fn min_both_mixed_test() {
  assert int.min(2, -2) == -2
}

pub fn min_both_negative_test() {
  assert int.min(-1, -1) == -1
}

pub fn max_equal_test() {
  assert int.max(0, 0) == 0
}

pub fn max_second_larger_test() {
  assert int.max(0, 1) == 1
}

pub fn max_first_larger_test() {
  assert int.max(1, 0) == 1
}

pub fn max_positive_larger_test() {
  assert int.max(-1, 2) == 2
}

pub fn max_mixed_test() {
  assert int.max(2, -2) == 2
}

pub fn max_both_negative_test() {
  assert int.max(-1, -1) == -1
}

pub fn is_even_zero_test() {
  assert int.is_even(0)
}

pub fn is_even_two_test() {
  assert int.is_even(2)
}

pub fn is_even_negative_two_test() {
  assert int.is_even(-2)
}

pub fn is_even_large_test() {
  assert int.is_even(10_006)
}

pub fn is_even_one_false_test() {
  assert !int.is_even(1)
}

pub fn is_even_negative_three_false_test() {
  assert !int.is_even(-3)
}

pub fn is_even_large_odd_false_test() {
  assert !int.is_even(10_005)
}

pub fn is_odd_zero_false_test() {
  assert !int.is_odd(0)
}

pub fn is_odd_two_false_test() {
  assert !int.is_odd(2)
}

pub fn is_odd_negative_two_false_test() {
  assert !int.is_odd(-2)
}

pub fn is_odd_large_even_false_test() {
  assert !int.is_odd(10_006)
}

pub fn is_odd_one_test() {
  assert int.is_odd(1)
}

pub fn is_odd_negative_three_test() {
  assert int.is_odd(-3)
}

pub fn is_odd_large_test() {
  assert int.is_odd(10_005)
}

pub fn power_two_squared_test() {
  assert int.power(2, 2.0) == Ok(4.0)
}

pub fn power_negative_cubed_test() {
  assert int.power(-5, 3.0) == Ok(-125.0)
}

pub fn power_zero_exponent_test() {
  assert int.power(10, 0.0) == Ok(1.0)
}

pub fn power_fractional_exponent_test() {
  assert int.power(16, 0.5) == Ok(4.0)
}

pub fn power_negative_exponent_test() {
  assert int.power(2, -1.0) == Ok(0.5)
}

pub fn power_negative_base_fractional_exponent_error_test() {
  // int.power(-1, 0.5) is equivalent to int.square_root(-1) and should
  // return an error as an imaginary number would otherwise have to be
  // returned
  assert int.power(-1, 0.5) == Error(Nil)
}

pub fn power_negative_base_fractional_exponent_error_2_test() {
  // Check another case with a negative base and fractional exponent
  assert int.power(-1, 1.5) == Error(Nil)
}

pub fn power_zero_base_negative_exponent_error_test() {
  // float.power(0, -1) is equivalent to 1 / 0 and is expected
  // to be an error
  assert int.power(0, -1.0) == Error(Nil)
}

pub fn power_negative_base_negative_exponent_test() {
  // Check that a negative base and exponent is fine as long as the
  // exponent is not fractional
  assert int.power(-2, -1.0) == Ok(-0.5)
}

pub fn square_root_four_test() {
  assert int.square_root(4) == Ok(2.0)
}

pub fn square_root_sixteen_test() {
  assert int.square_root(16) == Ok(4.0)
}

pub fn square_root_zero_test() {
  assert int.square_root(0) == Ok(0.0)
}

pub fn square_root_negative_error_test() {
  assert int.square_root(-4) == Error(Nil)
}

pub fn negate_negative_test() {
  assert int.negate(-1) == 1
}

pub fn negate_positive_test() {
  assert int.negate(2) == -2
}

pub fn negate_zero_test() {
  assert int.negate(0) == 0
}

pub fn sum_empty_test() {
  assert int.sum([]) == 0
}

pub fn sum_non_empty_test() {
  assert int.sum([1, 2, 3]) == 6
}

pub fn product_empty_test() {
  assert int.product([]) == 1
}

pub fn product_single_test() {
  assert int.product([4]) == 4
}

pub fn product_multiple_test() {
  assert int.product([1, 2, 3]) == 6
}

pub fn random_test() {
  use _, _ <- int.range(from: 0, to: 101, with: Nil)

  assert int.random(0) == 0

  assert int.random(1) == 0

  assert int.random(-1) == -1

  assert list.contains([0, 1], int.random(2))

  assert list.contains([0, 1, 2], int.random(3))

  Nil
}

pub fn divide_by_self_test() {
  assert int.divide(1, 1) == Ok(1)
}

pub fn divide_by_zero_test() {
  assert int.divide(1, 0) == Error(Nil)
}

pub fn divide_zero_by_one_test() {
  assert int.divide(0, by: 1) == Ok(0)
}

pub fn divide_one_by_zero_test() {
  assert int.divide(1, by: 0) == Error(Nil)
}

pub fn divide_truncates_test() {
  assert int.divide(5, by: 2) == Ok(2)
}

pub fn divide_negative_test() {
  assert int.divide(-99, by: 2) == Ok(-49)
}

pub fn remainder_basic_test() {
  assert int.remainder(3, 2) == Ok(1)
}

pub fn remainder_by_zero_test() {
  assert int.remainder(1, 0) == Error(Nil)
}

pub fn remainder_negative_divisor_test() {
  assert int.remainder(10, -1) == Ok(0)
}

pub fn remainder_positive_test() {
  assert int.remainder(13, by: 3) == Ok(1)
}

pub fn remainder_negative_dividend_test() {
  assert int.remainder(-13, by: 3) == Ok(-1)
}

pub fn remainder_positive_by_negative_test() {
  assert int.remainder(13, by: -3) == Ok(1)
}

pub fn remainder_both_negative_test() {
  assert int.remainder(-13, by: -3) == Ok(-1)
}

pub fn modulo_basic_test() {
  assert int.modulo(3, 2) == Ok(1)
}

pub fn modulo_by_zero_test() {
  assert int.modulo(1, 0) == Error(Nil)
}

pub fn modulo_negative_divisor_test() {
  assert int.modulo(10, -1) == Ok(0)
}

pub fn modulo_positive_test() {
  assert int.modulo(13, by: 3) == Ok(1)
}

pub fn modulo_negative_dividend_test() {
  assert int.modulo(-13, by: 3) == Ok(2)
}

pub fn modulo_positive_by_negative_test() {
  assert int.modulo(13, by: -3) == Ok(-2)
}

pub fn modulo_both_negative_test() {
  assert int.modulo(-13, by: -3) == Ok(-1)
}

pub fn floor_divide_by_self_test() {
  assert int.floor_divide(1, 1) == Ok(1)
}

pub fn floor_divide_by_zero_test() {
  assert int.floor_divide(1, 0) == Error(Nil)
}

pub fn floor_divide_zero_by_one_test() {
  assert int.floor_divide(0, by: 1) == Ok(0)
}

pub fn floor_divide_one_by_zero_test() {
  assert int.floor_divide(1, by: 0) == Error(Nil)
}

pub fn floor_divide_truncates_test() {
  assert int.floor_divide(5, by: 2) == Ok(2)
}

pub fn floor_divide_negative_divisor_test() {
  assert int.floor_divide(6, by: -4) == Ok(-2)
}

pub fn floor_divide_negative_dividend_test() {
  assert int.floor_divide(-99, by: 2) == Ok(-50)
}

pub fn floor_divide_negative_small_test() {
  assert int.floor_divide(-1, by: 2) == Ok(-1)
}

pub fn add_test() {
  assert int.add(1, 2) == 3
}

pub fn add_larger_test() {
  assert int.add(3, 2) == 5
}

pub fn multiply_test() {
  assert int.multiply(2, 4) == 8
}

pub fn multiply_larger_test() {
  assert int.multiply(3, 2) == 6
}

pub fn subtract_test() {
  assert int.subtract(3, 1) == 2
}

pub fn subtract_smaller_test() {
  assert int.subtract(3, 2) == 1
}

pub fn subtract_negative_result_test() {
  assert int.subtract(2, 3) == -1
}

pub fn bitwise_and_test() {
  assert int.bitwise_and(9, 3) == 1
}

pub fn bitwise_and_32bit_test() {
  // To check compatibility with JavaScript, try a 32 bit unsigned integer
  // (signed integers are in the range -2147483648 to +2147483647, while
  //  32 bit unsigned integers are in the range 0 to +4294967295).
  assert int.bitwise_and(2_147_483_648, 2_147_483_648) == 2_147_483_648
}

pub fn bitwise_not_test() {
  assert int.bitwise_not(2) == -3
}

pub fn bitwise_not_32bit_test() {
  // To check compatibility with JavaScript, try a 32 bit unsigned integer.
  assert int.bitwise_not(2_147_483_648) == -2_147_483_649
}

pub fn bitwise_or_test() {
  assert int.bitwise_or(9, 3) == 11
}

pub fn bitwise_or_32bit_test() {
  // To check compatibility with JavaScript, try a 32 bit unsigned integer.
  assert int.bitwise_or(1, 2_147_483_648) == 2_147_483_649
}

pub fn bitwise_exclusive_or_test() {
  assert int.bitwise_exclusive_or(9, 3) == 10
}

pub fn bitwise_exclusive_or_32bit_test() {
  // To check compatibility with JavaScript, try a 32 bit unsigned integer.
  assert int.bitwise_exclusive_or(0, 2_147_483_648) == 2_147_483_648
}

pub fn bitwise_shift_left_positive_test() {
  assert int.bitwise_shift_left(1, 2) == 4
}

pub fn bitwise_shift_left_negative_shift_test() {
  assert int.bitwise_shift_left(1, -2) == 0
}

pub fn bitwise_shift_left_negative_value_test() {
  assert int.bitwise_shift_left(-1, 2) == -4
}

pub fn bitwise_shift_left_both_negative_test() {
  assert int.bitwise_shift_left(-1, -2) == -1
}

pub fn bitwise_shift_right_positive_test() {
  assert int.bitwise_shift_right(1, 2) == 0
}

pub fn bitwise_shift_right_negative_shift_test() {
  assert int.bitwise_shift_right(1, -2) == 4
}

pub fn bitwise_shift_right_negative_value_test() {
  assert int.bitwise_shift_right(-1, 2) == -1
}

pub fn bitwise_shift_right_both_negative_test() {
  assert int.bitwise_shift_right(-1, -2) == -4
}

pub fn range_ascending_test() {
  assert int.range(from: 0, to: 3, with: "", run: fn(acc, i) {
      acc <> int.to_string(i)
    })
    == "012"
}

pub fn range_descending_test() {
  assert int.range(from: 3, to: 0, with: "", run: fn(acc, i) {
      acc <> int.to_string(i)
    })
    == "321"
}

pub fn range_empty_test() {
  assert int.range(from: 5, to: 5, with: "", run: fn(acc, i) {
      acc <> int.to_string(i)
    })
    == ""
}

pub fn range_with_list_prepend_test() {
  assert int.range(from: 1, to: -2, with: [], run: list.prepend) == [-1, 0, 1]
}

pub fn range_negative_to_positive_test() {
  assert int.range(from: -2, to: 2, with: [], run: fn(acc, i) { [i, ..acc] })
    == [1, 0, -1, -2]
}

pub fn range_single_element_test() {
  assert int.range(from: 0, to: 1, with: [], run: list.prepend) == [0]
}

pub fn range_sum_test() {
  assert int.range(from: 1, to: 5, with: 0, run: int.add) == 10
}
