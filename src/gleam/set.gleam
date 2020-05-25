import gleam/map.{Map}
import gleam/result

/// A set is a collection of unique elements of the same type.
///
/// It is implemented using the `gleam/map` module, so inserts and lookups have
/// logarithmic time complexity.
///
pub opaque type Set(element) {
  // A list is used as the map value as an empty list has the smallest
  // representation in Erlang's binary format
  Set(map: Map(element, List(Nil)))
}

/// Create a new empty set.
///
pub fn new() -> Set(element) {
  Set(map.new())
}

/// Get the number of elements in a set.
///
/// This function runs in constant time.
///
/// ## Examples
///
///    > new() |> insert(1) |> insert(2) |> size
///    2
///
pub fn size(set: Set(element)) -> Int {
  map.size(set.map)
}

/// Insert an element into the set.
///
/// This function runs in logarithmic time.
///
/// ## Examples
///
///    > new() |> insert(1) |> insert(2) |> size
///    2
///
pub fn insert(into set: Set(element), this element: element) -> Set(element) {
  Set(map: map.insert(set.map, element, []))
}

/// Check whether a set contains a given element.
///
/// This function runs in logarithmic time.
///
/// ## Examples
///
///    > new() |> insert(2) |> contains(2)
///    True
///
///    > new() |> insert(2) |> contains(1)
///    False
///
pub fn contains(in set: Set(element), this member: element) -> Bool {
  set.map
  |> map.get(member)
  |> result.is_ok
}

/// Remove an element from a set. If the set does not contain the element then
/// the set is returned unchanged.
///
/// ## Examples
///
///    > new() |> insert(2) |> delete(2) |> contains(1)
///    False
///
pub fn delete(from set: Set(element), this member: element) -> Set(element) {
  Set(map: map.delete(set.map, member))
}
