import gleam/map.{Map}
import gleam/result
import gleam/list

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
/// This function runs in logarithmic time.
///
/// ## Examples
///
///    > new() |> insert(2) |> delete(2) |> contains(1)
///    False
///
pub fn delete(from set: Set(element), this member: element) -> Set(element) {
  Set(map: map.delete(set.map, member))
}

/// Convert the set into a list of the contained elements.
///
/// The list has no specific ordering, any unintentional ordering may change in
/// future versions of Gleam or Erlang.
///
/// This function runs in linear time.
///
/// ## Examples
///
///    > new() |> insert(2) |> to_list
///    [2]
///
pub fn to_list(set: Set(element)) -> List(element) {
  map.keys(set.map)
}

/// Create a new set of the elements in a given list.
///
/// This function runs in loglinear time.
///
/// ## Examples
///
///    > import gleam/list
///    > [1, 1, 2, 4, 3, 2] |> from_list |> to_list |> list.sort
///    [1, 3, 3, 4]
///
pub fn from_list(elements: List(element)) -> Set(element) {
  let map = list.fold(
    over: elements,
    from: map.new(),
    with: fn(k, m) { map.insert(m, k, []) },
  )
  Set(map)
}
