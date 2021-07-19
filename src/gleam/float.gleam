import gleam/order.{Order}
  import gleam/string_builder

  pub type Float =
    Float

  /// Attempts to parse a string as a float, returning `Error(Nil)` if it was not
  /// possible.
  ///
  /// ## Examples
  ///    > parse("2.3")
  ///    Ok(2.3)
  ///
  ///    > parse("ABC")
  ///    Error(Nil)
  ///
if erlang {
  pub external fn parse(String) -> Result(Float, Nil) =
    "gleam_stdlib" "parse_float"
}
if javascript {
  pub external fn parse(String) -> Result(Float, Nil) =
    "../gleam_stdlib.js" "parse_float"
}

  /// Returns the string representation of the provided float.
  ///
  /// ## Examples
  ///    > to_string(2.3)
  ///    "2.3"
  ///
if erlang {
  pub fn to_string(f: Float) -> String {
    f
    |> string_builder.from_float
    |> string_builder.to_string
  }
}
if javascript {
  pub external fn to_string(f: Float) -> String =
    "../gleam_stdlib.js" "float_to_string"
}

  /// Restricts a Float between a lower and upper bound
  ///
  /// ## Examples
  ///
  /// ```
  /// > clamp(1.2, min: 1.4, max: 1.6)
  /// 1.4
  /// ```
  ///
  pub fn clamp(n: Float, min min_bound: Float, max max_bound: Float) -> Float {
    n
    |> min(max_bound)
    |> max(min_bound)
  }

  /// Compares two floats, returning an order.
  ///
  /// ## Examples
  ///    > compare(2.0, 2.3)
  ///    Lt
  ///
  pub fn compare(a: Float, with b: Float) -> Order {
    case a == b {
      True -> order.Eq
      False ->
        case a <. b {
          True -> order.Lt
          False -> order.Gt
        }
    }
  }

  /// Compares two floats, returning the smaller of the two.
  ///
  /// ## Examples
  ///
  ///    > min(2.0, 2.3)
  ///    2.0
  ///
  pub fn min(a: Float, b: Float) -> Float {
    case a <. b {
      True -> a
      False -> b
    }
  }

  /// Compares two floats, returning the larger of the two.
  ///
  /// ## Examples
  ///
  ///    > max(2.0, 2.3)
  ///    2.3
  ///
  pub fn max(a: Float, b: Float) -> Float {
    case a >. b {
      True -> a
      False -> b
    }
  }

  /// Rounds the value to the next highest whole number as a float.
  ///
  /// ## Examples
  ///
  ///    > ceiling(2.3)
  ///    3.0
  ///
if erlang {
  pub external fn ceiling(Float) -> Float =
    "math" "ceil"
}
if javascript {
  pub external fn ceiling(Float) -> Float =
    "../gleam_stdlib.js" "ceiling"

}


  /// Rounds the value to the next lowest whole number as a float.
  ///
  /// ## Examples
  ///
  ///    > floor(2.3)
  ///    2.0
  ///
if erlang {
  pub external fn floor(Float) -> Float =
    "math" "floor"
}
if javascript {
  pub external fn floor(Float) -> Float =
    "../gleam_stdlib.js" "floor"

}

  /// Rounds the value to the nearest whole number as an int.
  ///
  /// ## Examples
  ///
  ///    > round(2.3)
  ///    2
  ///
  ///    > round(2.5)
  ///    3
  ///
if erlang {
  pub external fn round(Float) -> Int =
    "erlang" "round"
}
if javascript {
  pub external fn round(Float) -> Int =
    "../gleam_stdlib.js" "round"
}

  /// Returns the value as an int, truncating all decimal digits.
  ///
  /// ## Examples
  ///
  ///    > truncate(2.4343434847383438)
  ///    2
  ///
if erlang {
  pub external fn truncate(Float) -> Int =
    "erlang" "trunc"
}
if javascript {
  pub external fn truncate(Float) -> Int =
    "../gleam_stdlib.js" "truncate"
}
  /// Returns the absolute value of the input as a float.
  ///
  /// ## Examples
  ///
  ///    > absolute_value(-12.5)
  ///    12.5
  ///
  ///    > absolute_value(10.2)
  ///    10.2
  ///
if erlang {
  pub external fn absolute_value(Float) -> Float =
    "erlang" "abs"
}
if javascript {
   pub external fn absolute_value(Float) -> Int =
    "../gleam_stdlib.js" "absolute_value"
}

  /// Returns the results of the base being raised to the power of the
  /// exponent, as a float.
  ///
  /// ## Examples
  ///
  ///    > power(2.0, 2.0)
  ///    4.0
  ///
  ///    > power(8.0, 1.5)
  ///    64.0
  ///
if erlang {
  pub external fn power(base: Float, exponent: Float) -> Float =
    "math" "pow"
}
if javascript {
  pub external fn power(base: Float, exponent: Float) -> Float =
    "../gleam_stdlib.js" "power"

}

  /// Returns the square root of the input as a float.
  ///
  /// ## Examples
  ///
  ///    > square_root(4.0)
  ///    Ok(2.0)
  ///
  ///    > square_root(-16.0)
  ///    Error(Nil)
  ///
  pub fn square_root(number: Float) -> Result(Float, Nil) {
    case number <. 0.0 {
      True -> Error(Nil)
      False -> Ok(power(number, 0.5))
    }
  }

  /// Returns the negative of the value provided
  ///
  /// ## Examples
  ///
  ///    > negate(1.)
  ///    -1.
  ///
  pub fn negate(x: Float) -> Float {
    -1. *. x
  }

  /// Sums a list of Floats.
  ///
  /// ## Example
  ///
  ///    > sum([1.0, 2.2, 3.3])
  ///    6.5
  ///
  pub fn sum(numbers: List(Float)) -> Float {
    numbers
    |> do_sum(0.0)
  }

  fn do_sum(numbers: List(Float), initial: Float) -> Float {
    case numbers {
      [] -> initial
      [x, ..rest] -> do_sum(rest, x +. initial)
    }
  }

  /// Multiplies a list of Floats and returns the product.
  ///
  /// ## Example
  ///
  ///    > product([2.5, 3.2, 4.2])
  ///    33.6
  ///
  pub fn product(numbers: List(Float)) -> Float {
    case numbers {
      [] -> 0.
      _ -> do_product(numbers, 1.)
    }
  }

  fn do_product(numbers: List(Float), initial: Float) -> Float {
    case numbers {
      [] -> initial
      [x, ..rest] -> do_product(rest, x *. initial)
    }
  }
