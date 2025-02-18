import gleam/bytes_tree
import gleam/should

pub fn tree_unaligned_bit_arrays_test() {
  let data =
    bytes_tree.from_bit_array(<<-1:5>>)
    |> bytes_tree.append(<<-1:3>>)
    |> bytes_tree.append(<<-2:2>>)
    |> bytes_tree.prepend(<<-1:4>>)

  data
  |> bytes_tree.to_bit_array
  |> should.equal(<<-1:4, 0:4, -1:5, 0:3, -1:3, 0:5, -2:2, 0:6>>)

  data
  |> bytes_tree.byte_size
  |> should.equal(4)
}

pub fn concat_unaligned_bit_arrays_test() {
  bytes_tree.concat_bit_arrays([<<-1:4>>, <<-1:5>>, <<-1:3>>, <<-2:2>>])
  |> bytes_tree.to_bit_array
  |> should.equal(<<-1:4, 0:4, -1:5, 0:3, -1:3, 0:5, -2:2, 0:6>>)
}
