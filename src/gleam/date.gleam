/// Date is type that is similar in functionality to the Erlang calendar module,
/// but the types do not directly interface with the calendar module.
/// To convert an erlang date to a Date type, use `from_tuple/1`
///
/// Currently this module defaults to UTC.
///
/// Years cannot be abreviated.  Years start from 0 and go up.  So if the year = 93,
/// it does not refer to 1993.  This is in line with the behavior of Erlang.
///
import gleam/result
import gleam/int

pub type Date {
  Date(year: Int, month: Int, day: Int)
}

/// Gets the UTC timestamp.
///
/// ## Example
///
/// ```gleam
/// universal_time() = tuple(tuple(year, month, day), tuple(hour, minute, second))
/// ```
///
pub external fn universal_time() 
  -> tuple(tuple(Int, Int, Int), tuple(Int, Int, Int)) = "calendar" "universal_time"

/// Gets the current Date.
///
/// ## Example
///
/// ```gleam
/// current_date == Date(2020, 04, 22)
/// ```
///
pub fn current_date() -> Date() {
  let tuple(tuple(year, month, day), _time_tuple) = universal_time()
  Date(year, month, day)
}


/// Returns the last day of the pass year on the passed month.
/// Using erlang's calendar module.
///
/// ## Examples
/// ```
/// last_day_of_the_month(2020, 04) == 30
/// ```
///
pub external fn last_day_of_the_month(year: Int, month: Int) 
  -> Int = "calendar" "last_day_of_the_month"

/// Checks to see if the current year is a leap year.
///
/// ## Examples
/// ```
/// is_leap_year(2020) == True
/// is_leap_year(2019) == False
/// ```
///
pub fn is_leap_year(year: Int) -> Bool{
  case year % 4 == 0, year % 100 > 0, year % 400 == 0 {
    True, True, _ -> True
    _, _, True -> True
    _, _, _ -> False
  }
}

/// Checks to see if the passed date is valid.
///
/// ## Examples
/// ```gleam
/// let date = Date(12, 31, 2020)
/// is_valid_date(date) == True
/// 
/// let invalid_date = Date(99, 99, -1)
/// is_valid_date(invalid_date) == False
/// ```
///
pub fn is_valid_date(year: Int, month: Int, day: Int) -> Bool {
  case year >= 0 && month < 12 && month > 0 && day > 0 {
    True -> day <= last_day_of_the_month(year, month)
    False -> False
  }
}

/// Checks to see if the passed date is valid.
///
/// ## Examples
/// ```gleam
/// let date = Date(12, 31, 2020)
/// is_valid_date(date) == True
/// 
/// let invalid_date = Date(99, 99, -1)
/// is_valid_date(invalid_date) == False
/// ```
///
pub fn is_valid_date_type(date: Date) -> Bool {
  let Date(year, month, day) = date
  is_valid_date(year, month, day)
}

/// Safely create a valid Date.
///
/// ## Examples
/// ```gleam
/// new(2020, 04, 20) == Ok(Date((2020, 04, 20))
/// new(-1, -1, -1) == Error("Parameters are invalid for a date.")
/// ```
///
pub fn new(year: Int, month: Int, day: Int) -> Result(Date, String) {
  let valid_date = is_valid_date(year, month, day)

  case valid_date {
    True -> Ok(Date(year, month, day))
    False -> Error("Parameters are invalid for a date.")
  }
}

/// Safely create a valid Date from an erlang date.
///
/// ## Examples
/// ```gleam
/// from_tuple(tuple(2020, 04, 20)) == Ok(Date((2020, 04, 20))
/// from_tuple(tuple(-1, -1, -1)) == Error("Parameters are invalid for a date.")
/// ```
///
pub fn from_tuple(date_tuple: tuple(Int, Int, Int)) -> Result(Date, String) {
  let tuple(year, month, day) = date_tuple
  let valid_date = is_valid_date(year, month, day)

  case valid_date {
    True -> Ok(Date(year, month, day))
    False -> Error("Parameters are invalid for a date.")
  }
}
