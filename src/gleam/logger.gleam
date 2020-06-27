import gleam/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/map.{Map}

pub type Level {
  Emergency
  Alert
  Critical
  Error
  Warning
  Notice
  Info
  Debug
}

type Ok {
  Ok
}

external fn erl_log(Level, List(tuple(Atom, Dynamic))) -> Ok =
  "logger" "log"

external fn erl_update_process_metadata(Map(Atom, Dynamic)) -> Ok =
  "logger" "update_process_metadata"

// Better name? entry/item/statement
pub fn field(key: String, value: a) -> tuple(Atom, Dynamic) {
  tuple(atom.create_from_string(key), dynamic.from(value))
}

pub fn log(level, report) -> Nil {
  erl_log(level, report)
  Nil
}

pub fn add_metadata(key, value) -> Nil {
  [tuple(atom.create_from_string(key), dynamic.from(value))]
  |> map.from_list()
  |> erl_update_process_metadata()
  Nil
}
