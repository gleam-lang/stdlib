import gleam/float
import gleam/int
import gleam/order
import gleam/result

pub fn parse_positive_test() {
  assert float.parse("1.23") == Ok(1.23)
}

pub fn parse_positive_with_plus_test() {
  assert float.parse("+1.23") == Ok(1.23)
}

pub fn parse_negative_test() {
  assert float.parse("-1.23") == Ok(-1.23)
}

pub fn parse_whole_test() {
  assert float.parse("5.0") == Ok(5.0)
}

pub fn parse_many_decimal_places_test() {
  assert float.parse("0.123456789") == Ok(0.123456789)
}

pub fn parse_scientific_test() {
  assert float.parse("1.234e10") == Ok(1.234e10)
}

pub fn parse_scientific_negative_positive_test() {
  assert float.parse("-1.234e+10") == Ok(-1.234e10)
}

pub fn parse_scientific_negative_exponent_test() {
  assert float.parse("1.234e-10") == Ok(1.234e-10)
}

pub fn parse_empty_test() {
  assert float.parse("") == Error(Nil)
}

pub fn parse_invalid_test() {
  assert float.parse("what") == Error(Nil)
}

pub fn parse_integer_test() {
  assert float.parse("1") == Error(Nil)
}

pub fn to_string_zero_test() {
  assert float.to_string(0.0) == "0.0"
}

pub fn to_string_small_positive_test() {
  assert float.to_string(0.0123) == "0.0123"
}

pub fn to_string_small_negative_test() {
  assert float.to_string(-0.0123) == "-0.0123"
}

pub fn to_string_positive_test() {
  assert float.to_string(12.67) == "12.67"
}

pub fn to_string_negative_test() {
  assert float.to_string(-12.67) == "-12.67"
}

pub fn to_string_whole_positive_test() {
  assert float.to_string(123.0) == "123.0"
}

pub fn to_string_whole_negative_test() {
  assert float.to_string(-123.0) == "-123.0"
}

pub fn to_string_scientific_positive_test() {
  assert float.to_string(3.0e26) == "3.0e26"
}

pub fn to_string_scientific_negative_test() {
  assert float.to_string(-3.0e26) == "-3.0e26"
}

pub fn to_string_scientific_small_positive_test() {
  assert float.to_string(3.0e-26) == "3.0e-26"
}

pub fn to_string_scientific_small_negative_test() {
  assert float.to_string(-3.0e-26) == "-3.0e-26"
}

pub fn to_string_scientific_large_test() {
  assert float.to_string(456.12e78) == "4.5612e80"
}

pub fn to_string_scientific_large_negative_test() {
  assert float.to_string(-456.12e78) == "-4.5612e80"
}

pub fn to_string_scientific_small_test() {
  assert float.to_string(456.12e-78) == "4.5612e-76"
}

pub fn to_string_scientific_small_negative_2_test() {
  assert float.to_string(-456.12e-78) == "-4.5612e-76"
}

pub fn clamp_within_range_test() {
  assert float.clamp(1.4, min: 1.3, max: 1.5) == 1.4
}

pub fn clamp_below_min_test() {
  assert float.clamp(1.2, min: 1.3, max: 1.5) == 1.3
}

pub fn clamp_above_max_test() {
  assert float.clamp(1.6, min: 1.3, max: 1.5) == 1.5
}

pub fn clamp_inverted_range_test() {
  assert float.clamp(1.2, min: 1.4, max: 0.6) == 1.2
}

pub fn compare_equal_zero_test() {
  assert float.compare(0.0, 0.0) == order.Eq
}

pub fn compare_equal_test() {
  assert float.compare(0.1, 0.1) == order.Eq
}

pub fn compare_lt_test() {
  assert float.compare(0.0, 0.1) == order.Lt
}

pub fn compare_negative_lt_test() {
  assert float.compare(-2.0, -1.9) == order.Lt
}

pub fn compare_gt_test() {
  assert float.compare(2.0, 1.9) == order.Gt
}

pub fn compare_negative_gt_test() {
  assert float.compare(-1.9, -2.0) == order.Gt
}

pub fn loosely_compare_lt_no_tolerance_test() {
  assert float.loosely_compare(10.2, 10.5, tolerating: 0.0) == order.Lt
}

pub fn loosely_compare_eq_with_tolerance_test() {
  assert float.loosely_compare(10.2, with: 10.5, tolerating: 0.31) == order.Eq
}

pub fn loosely_compare_eq_reversed_test() {
  assert float.loosely_compare(10.5, 10.2, 0.31) == order.Eq
}

pub fn loosely_compare_lt_insufficient_tolerance_test() {
  assert float.loosely_compare(10.2, 10.5, 0.29) == order.Lt
}

pub fn loosely_compare_gt_insufficient_tolerance_test() {
  assert float.loosely_compare(10.5, 10.2, 0.29) == order.Gt
}

pub fn loosely_compare_negative_eq_test() {
  assert float.loosely_compare(-10.2, -10.5, 0.31) == order.Eq
}

pub fn loosely_equals_no_tolerance_test() {
  assert !float.loosely_equals(10.2, 10.5, tolerating: 0.0)
}

pub fn loosely_equals_with_tolerance_test() {
  assert float.loosely_equals(10.2, with: 10.5, tolerating: 0.31)
}

pub fn loosely_equals_reversed_test() {
  assert float.loosely_equals(10.5, 10.2, 0.31)
}

pub fn loosely_equals_insufficient_tolerance_first_test() {
  assert !float.loosely_equals(10.2, 10.5, 0.29)
}

pub fn loosely_equals_insufficient_tolerance_second_test() {
  assert !float.loosely_equals(10.5, 10.2, 0.29)
}

pub fn loosely_equals_negative_test() {
  assert float.loosely_equals(-10.2, -10.5, 0.31)
}

pub fn ceiling_positive_test() {
  assert float.ceiling(8.1) == 9.0
}

pub fn ceiling_negative_test() {
  assert float.ceiling(-8.1) == -8.0
}

pub fn ceiling_whole_negative_test() {
  assert float.ceiling(-8.0) == -8.0
}

pub fn floor_positive_test() {
  assert float.floor(8.1) == 8.0
}

pub fn floor_negative_test() {
  assert float.floor(-8.1) == -9.0
}

pub fn floor_whole_negative_test() {
  assert float.floor(-8.0) == -8.0
}

pub fn round_down_test() {
  assert float.round(8.1) == 8
}

pub fn round_down_4_test() {
  assert float.round(8.4) == 8
}

pub fn round_down_499_test() {
  assert float.round(8.499) == 8
}

pub fn round_up_test() {
  assert float.round(8.5) == 9
}

pub fn round_negative_test() {
  assert float.round(-8.1) == -8
}

pub fn round_negative_half_test() {
  assert float.round(-7.5) == -8
}

pub fn truncate_positive_1_test() {
  assert float.truncate(8.1) == 8
}

pub fn truncate_positive_4_test() {
  assert float.truncate(8.4) == 8
}

pub fn truncate_positive_499_test() {
  assert float.truncate(8.499) == 8
}

pub fn truncate_positive_5_test() {
  assert float.truncate(8.5) == 8
}

pub fn truncate_negative_1_test() {
  assert float.truncate(-8.1) == -8
}

pub fn truncate_negative_5_test() {
  assert float.truncate(-7.5) == -7
}

pub fn to_precision_round_down_test() {
  assert float.to_precision(2.43434348473, 2) == 2.43
}

pub fn to_precision_round_up_test() {
  assert float.to_precision(2.43534348473, 2) == 2.44
}

pub fn to_precision_negative_round_up_test() {
  assert float.to_precision(-2.43534348473, 2) == -2.44
}

pub fn to_precision_negative_precision_up_test() {
  assert float.to_precision(547_890.453444, -3) == 548_000.0
}

pub fn to_precision_negative_precision_down_test() {
  assert float.to_precision(547_490.453444, -3) == 547_000.0
}

pub fn to_precision_negative_both_test() {
  assert float.to_precision(-547_490.453444, -3) == -547_000.0
}

pub fn to_precision_zero_test() {
  assert float.to_precision(435.3224, 0) == 435.0
}

pub fn to_precision_negative_zero_test() {
  assert float.to_precision(435.3224, -0) == 435.0
}

pub fn to_precision_floating_point_fix_test() {
  assert float.to_precision(184.20000000000002, 2) == 184.2
}

pub fn to_precision_large_test() {
  assert float.to_precision(12_345_678_912_345_678_912_345_678.0, -19)
    == 1_234_568.0e19
}

pub fn min_equal_test() {
  assert float.min(0.0, 0.0) == 0.0
}

pub fn min_first_smaller_test() {
  assert float.min(0.3, 1.5) == 0.3
}

pub fn min_second_smaller_test() {
  assert float.min(1.0, 0.0) == 0.0
}

pub fn min_negative_smaller_test() {
  assert float.min(-1.7, 2.5) == -1.7
}

pub fn min_both_negative_equal_test() {
  assert float.min(-2.2, -2.2) == -2.2
}

pub fn min_both_negative_equal_2_test() {
  assert float.min(-1.0, -1.0) == -1.0
}

pub fn min_both_negative_test() {
  assert float.min(-1.1, -1.0) == -1.1
}

pub fn max_equal_test() {
  assert float.max(0.0, 0.0) == 0.0
}

pub fn max_second_larger_test() {
  assert float.max(0.3, 1.5) == 1.5
}

pub fn max_first_larger_test() {
  assert float.max(1.0, 0.0) == 1.0
}

pub fn max_positive_larger_test() {
  assert float.max(-1.7, 2.5) == 2.5
}

pub fn max_both_negative_equal_test() {
  assert float.max(-2.2, -2.2) == -2.2
}

pub fn max_both_negative_equal_2_test() {
  assert float.max(-1.0, -1.0) == -1.0
}

pub fn max_both_negative_test() {
  assert float.max(-1.1, -1.0) == -1.0
}

pub fn absolute_value_negative_test() {
  assert float.absolute_value(-1.0) == 1.0
}

pub fn absolute_value_negative_large_test() {
  assert float.absolute_value(-20.6) == 20.6
}

pub fn absolute_value_zero_test() {
  assert float.absolute_value(0.0) == 0.0
}

pub fn absolute_value_positive_test() {
  assert float.absolute_value(1.0) == 1.0
}

pub fn absolute_value_positive_large_test() {
  assert float.absolute_value(25.2) == 25.2
}

pub fn power_two_squared_test() {
  assert float.power(2.0, 2.0) == Ok(4.0)
}

pub fn power_negative_cubed_test() {
  assert float.power(-5.0, 3.0) == Ok(-125.0)
}

pub fn power_zero_exponent_test() {
  assert float.power(10.5, 0.0) == Ok(1.0)
}

pub fn power_fractional_exponent_test() {
  assert float.power(16.0, 0.5) == Ok(4.0)
}

pub fn power_negative_exponent_test() {
  assert float.power(2.0, -1.0) == Ok(0.5)
}

pub fn power_negative_exponent_2_test() {
  assert float.power(2.0, -1.0) == Ok(0.5)
}

pub fn power_negative_base_fractional_exponent_error_test() {
  // float.power(-1.0, 0.5) is equivalent to float.square_root(-1.0)
  // and should return an error as an imaginary number would otherwise
  // have to be returned
  assert float.power(-1.0, 0.5) == Error(Nil)
}

pub fn power_negative_base_fractional_exponent_error_2_test() {
  // Check another case with a negative base and fractional exponent
  assert float.power(-1.5, 1.5) == Error(Nil)
}

pub fn power_zero_base_negative_exponent_error_test() {
  // float.power(0.0, -1.0) is equivalent to 1. /. 0 and is expected
  // to be an error
  assert float.power(0.0, -1.0) == Error(Nil)
}

pub fn power_negative_base_negative_exponent_test() {
  // Check that a negative base and exponent is fine as long as the
  // exponent is not fractional
  assert float.power(-2.0, -1.0) == Ok(-0.5)
}

pub fn square_root_four_test() {
  assert float.square_root(4.0) == Ok(2.0)
}

pub fn square_root_sixteen_test() {
  assert float.square_root(16.0) == Ok(4.0)
}

pub fn square_root_zero_test() {
  assert float.square_root(0.0) == Ok(0.0)
}

pub fn square_root_negative_error_test() {
  assert float.square_root(-4.0) == Error(Nil)
}

pub fn negate_negative_test() {
  assert float.negate(-1.0) == 1.0
}

pub fn negate_positive_test() {
  assert float.negate(2.0) == -2.0
}

pub fn negate_double_test() {
  assert float.negate(float.negate(0.0)) == 0.0
}

pub fn sum_empty_test() {
  assert float.sum([]) == 0.0
}

pub fn sum_non_empty_test() {
  assert float.sum([1.0, 2.2, 3.3]) == 6.5
}

pub fn product_empty_test() {
  assert float.product([]) == 1.0
}

pub fn product_single_test() {
  assert float.product([4.0]) == 4.0
}

pub fn product_multiple_test() {
  assert float.product([2.5, 3.2, 4.2]) == 33.6
}

pub fn random_test() {
  let expected_average = 0.5
  let iterations = 10_000
  let sum =
    int.range(from: 0, to: iterations + 1, with: 0.0, run: fn(accumulator, _) {
      let i = float.random()

      assert { i <. 1.0 }
      assert { i >=. 0.0 }

      accumulator +. i
    })
  let average = sum /. int.to_float(iterations)

  assert { average <. expected_average +. 0.1 }
  assert { average >. expected_average -. 0.1 }
}

pub fn modulo_by_zero_test() {
  assert float.modulo(13.3, by: 0.0) == Error(Nil)
}

pub fn modulo_positive_test() {
  assert float.modulo(13.3, by: 3.3)
    |> result.unwrap(or: 0.0)
    |> float.loosely_equals(with: 0.1, tolerating: 0.001)
}

pub fn modulo_negative_dividend_test() {
  assert float.modulo(-13.3, by: 3.3)
    |> result.unwrap(or: 0.0)
    |> float.loosely_equals(with: 3.2, tolerating: 0.001)
}

pub fn modulo_negative_divisor_test() {
  assert float.modulo(13.3, by: -3.3)
    |> result.unwrap(or: 0.0)
    |> float.loosely_equals(with: -3.2, tolerating: 0.001)
}

pub fn modulo_both_negative_test() {
  assert float.modulo(-13.3, by: -3.3)
    |> result.unwrap(or: 0.0)
    |> float.loosely_equals(with: -0.1, tolerating: 0.001)
}

pub fn divide_by_self_test() {
  assert float.divide(1.0, 1.0) == Ok(1.0)
}

pub fn divide_by_zero_test() {
  assert float.divide(1.0, 0.0) == Error(Nil)
}

pub fn divide_zero_by_one_test() {
  assert float.divide(0.0, by: 1.0) == Ok(0.0)
}

pub fn divide_one_by_zero_test() {
  assert float.divide(1.0, by: 0.0) == Error(Nil)
}

pub fn add_test() {
  assert float.add(1.0, 2.0) == 3.0
}

pub fn add_larger_test() {
  assert float.add(3.0, 2.0) == 5.0
}

pub fn multiply_test() {
  assert float.multiply(2.0, 4.0) == 8.0
}

pub fn multiply_larger_test() {
  assert float.multiply(3.0, 2.0) == 6.0
}

pub fn subtract_test() {
  assert float.subtract(3.0, 1.0) == 2.0
}

pub fn subtract_smaller_test() {
  assert float.subtract(3.0, 2.0) == 1.0
}

pub fn subtract_negative_result_test() {
  assert float.subtract(2.0, 3.0) == -1.0
}

pub fn logarithm_one_test() {
  assert float.logarithm(1.0)
    |> result.unwrap(or: 1.0)
    |> float.loosely_equals(with: 0.0, tolerating: 0.001)
}

pub fn logarithm_e_test() {
  assert float.logarithm(2.718281828459045)
    |> result.unwrap(or: 0.0)
    |> float.loosely_equals(with: 1.0, tolerating: 0.001)
}

pub fn logarithm_ten_test() {
  assert float.logarithm(10.0)
    |> result.unwrap(or: 0.0)
    |> float.loosely_equals(with: 2.302585092994046, tolerating: 0.001)
}

pub fn logarithm_hundred_test() {
  assert float.logarithm(100.0)
    |> result.unwrap(or: 0.0)
    |> float.loosely_equals(with: 4.605170185988092, tolerating: 0.001)
}

pub fn logarithm_half_test() {
  assert float.logarithm(0.5)
    |> result.unwrap(or: 0.0)
    |> float.loosely_equals(with: -0.6931471805599453, tolerating: 0.001)
}

pub fn logarithm_tenth_test() {
  assert float.logarithm(0.1)
    |> result.unwrap(or: 0.0)
    |> float.loosely_equals(with: -2.3025850929940455, tolerating: 0.001)
}

pub fn logarithm_zero_error_test() {
  assert float.logarithm(0.0) == Error(Nil)
}

pub fn logarithm_negative_one_error_test() {
  assert float.logarithm(-1.0) == Error(Nil)
}

pub fn logarithm_negative_hundred_error_test() {
  assert float.logarithm(-100.0) == Error(Nil)
}

pub fn logarithm_negative_tenth_error_test() {
  assert float.logarithm(-0.1) == Error(Nil)
}

pub fn exponential_zero_test() {
  assert float.loosely_equals(
    float.exponential(0.0),
    with: 1.0,
    tolerating: 0.001,
  )
}

pub fn exponential_one_test() {
  assert float.loosely_equals(
    float.exponential(1.0),
    with: 2.718281828459045,
    tolerating: 0.001,
  )
}

pub fn exponential_two_test() {
  assert float.loosely_equals(
    float.exponential(2.0),
    with: 7.38905609893065,
    tolerating: 0.001,
  )
}

pub fn exponential_negative_one_test() {
  assert float.loosely_equals(
    float.exponential(-1.0),
    with: 0.36787944117144233,
    tolerating: 0.001,
  )
}

pub fn exponential_five_test() {
  assert float.loosely_equals(
    float.exponential(5.0),
    with: 148.4131591025766,
    tolerating: 0.001,
  )
}

pub fn exponential_negative_five_test() {
  assert float.loosely_equals(
    float.exponential(-5.0),
    with: 0.006737946999085467,
    tolerating: 0.001,
  )
}

pub fn exponential_small_test() {
  assert float.loosely_equals(
    float.exponential(0.000001),
    with: 1.0000010000005,
    tolerating: 0.001,
  )
}

pub fn exponential_large_negative_test() {
  assert float.loosely_equals(
    float.exponential(-100.0),
    with: 3.720075976020836e-44,
    tolerating: 0.001,
  )
}
