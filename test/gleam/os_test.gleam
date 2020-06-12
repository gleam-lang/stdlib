import gleam/map
import gleam/os
import gleam/io
import gleam/should

pub fn env_test() {
  os.insert_env("GLEAM_TEST", "hello")
  os.get_env()
  |> map.get("GLEAM_TEST")
  |> should.equal(Ok("hello"))

  os.delete_env("GLEAM_TEST")
  os.get_env()
  |> map.get("GLEAM_TEST")
  |> should.equal(Error(Nil))
}

pub fn system_time_test() {
  let june_12_2020 = 1591966971
  { os.system_time(os.Second) > june_12_2020 }
  |> should.equal(True)
  { os.system_time(os.Second) < june_12_2020 * 1000 }
  |> should.equal(True)
  { os.system_time(os.Millisecond) > june_12_2020 * 1000 }
  |> should.equal(True)
  { os.system_time(os.Millisecond) < june_12_2020 * 1000000 }
  |> should.equal(True)
}
