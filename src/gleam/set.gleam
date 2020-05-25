import gleam/map.{Map}
import gleam/result
import gleam/list

/// A set is a collection of unique members of the same type.
///
/// It is implemented using the `gleam/map` module, so inserts and lookups have
/// logarithmic time complexity.
///
pub opaque type Set(member) {
  // A list is used as the map value as an empty list has the smallest
  // representation in Erlang's binary format
  Set(map: Map(member, List(Nil)))
}

/// Create a new empty set.
///
pub fn new() -> Set(member) {
  Set(map.new())
}

/// Get the number of members in a set.
///
/// This function runs in constant time.
///
/// ## Examples
///
///    > new() |> insert(1) |> insert(2) |> size
///    2
///
pub fn size(set: Set(member)) -> Int {
  map.size(set.map)
}

/// Insert an member into the set.
///
/// This function runs in logarithmic time.
///
/// ## Examples
///
///    > new() |> insert(1) |> insert(2) |> size
///    2
///
pub fn insert(into set: Set(member), this member: member) -> Set(member) {
  Set(map: map.insert(set.map, member, []))
}

/// Check whether a set contains a given member.
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
pub fn contains(in set: Set(member), this member: member) -> Bool {
  set.map
  |> map.get(member)
  |> result.is_ok
}

/// Remove an member from a set. If the set does not contain the member then
/// the set is returned unchanged.
///
/// This function runs in logarithmic time.
///
/// ## Examples
///
///    > new() |> insert(2) |> delete(2) |> contains(1)
///    False
///
pub fn delete(from set: Set(member), this member: member) -> Set(member) {
  Set(map: map.delete(set.map, member))
}

/// Convert the set into a list of the contained members.
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
pub fn to_list(set: Set(member)) -> List(member) {
  map.keys(set.map)
}

/// Create a new set of the members in a given list.
///
/// This function runs in loglinear time.
///
/// ## Examples
///
///    > import gleam/list
///    > [1, 1, 2, 4, 3, 2] |> from_list |> to_list |> list.sort
///    [1, 3, 3, 4]
///
pub fn from_list(members: List(member)) -> Set(member) {
  let map = list.fold(
    over: members,
    from: map.new(),
    with: fn(k, m) { map.insert(m, k, []) },
  )
  Set(map)
}

/// Combine all entries into a single value by calling a given function on each
/// one.
///
/// Sets are not ordered so the values are not returned in any specific order.
/// Do not write code that relies on the order entries are used by this
/// function as it may change in later versions of Gleam or Erlang.
///
/// # Examples
///
///    > from_list([1, 3, 9])
///    > |> fold(0, fn(member, accumulator) { accumulator + member })
///    13
///
pub fn fold(
  over set: Set(member),
  from initial: acc,
  with reducer: fn(member, acc) -> acc,
) -> acc {
  map.fold(over: set.map, from: initial, with: fn(k, _, a) { reducer(k, a) })
}
