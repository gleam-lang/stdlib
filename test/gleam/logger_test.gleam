import gleam/logger.{log, field, Error, Warning}

pub fn logger_test() {
  log(
    Warning,
    [
      field("event", "test_warning"),
      field("number", 5),
      field(
        "really_long_field",
        "00000000000000000000000000000000000000000000000000000",
      ),
    ],
  )
  todo
}
