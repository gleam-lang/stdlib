import gleam/map
import gleam/os
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
