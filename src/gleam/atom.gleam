/// A datatype and related functions for working with Erlang atoms.

pub external type Atom;

/// Error returned when a given Atom does not currently exist
pub type AtomNotLoaded {
  AtomNotLoaded
}

/// Will return a value of type `Atom` that represents a specific Erlang
/// atom from whom the string is its text representation if it exists.
///
pub external fn from_string(String) -> Result(Atom, AtomNotLoaded) =
  "gleam_stdlib" "atom_from_string";

/// This function can create a new atom if one does not already exist for
/// the given string. Atoms are not garbage collected so this can result
/// in a memory leak if called over time on new values
///
pub external fn create_from_string(String) -> Atom =
  "gleam_stdlib" "atom_create_from_string";

/// Retuns a `String` corresponding to the text representation of the given
/// `Atom`
///
pub external fn to_string(Atom) -> String =
  "gleam_stdlib" "atom_to_string";
