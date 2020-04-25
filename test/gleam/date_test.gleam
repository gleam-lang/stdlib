import gleam/date.{Date, new}
import gleam/should
import gleam/result

pub fn new_test() {
  date.new(2020, 04, 20)
  |> should.equal(_, Ok(Date(2020, 04, 20)))

  date.new(-1, 10, 10)
  |> should.equal(_, Error("Parameters are invalid for a date."))

  date.new(2020, -1, 20)
  |> should.equal(_, Error("Parameters are invalid for a date."))

  date.new(2020, 300, 20)
  |> should.equal(_, Error("Parameters are invalid for a date."))

  date.new(2020, 04, -1)
  |> should.equal(_, Error("Parameters are invalid for a date."))

  date.new(2020, 04, 300)
  |> should.equal(_, Error("Parameters are invalid for a date."))
}

pub fn from_tuple_test() {
  tuple(2020, 04, 20)
  |> date.from_tuple(_)
  |> should.equal(_, Ok(Date(2020, 04, 20)))

  tuple(-1, -1, -1)
  |> date.from_tuple(_)
  |> should.equal(_, Error("Parameters are invalid for a date."))
}

pub fn is_leap_year_test() {
  date.is_leap_year(2020)
  |> should.equal(_, True)

  date.is_leap_year(2019)
  |> should.equal(_, False)
}

pub fn last_day_of_the_month_test() {
  date.last_day_of_the_month(2020, 04)
  |> should.equal(_, 30)

  date.last_day_of_the_month(1993, 12)
  |> should.equal(_, 31)
}

pub fn is_valid_date_test() {
  Date(2020, 04, 20)
  |> date.is_valid_date_type(_)
  |> should.equal(_, True)

  Date(-1, -1, -1)
  |> date.is_valid_date_type(_)
  |> should.equal(_, False)
}
