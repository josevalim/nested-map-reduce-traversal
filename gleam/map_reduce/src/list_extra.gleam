import gleam/list

// This could live in the standard library
pub fn map_reduce(over items, from acc, with f) {
  let reducer = fn(elem, acc) {
    let tuple(mapped, inner_acc) = acc
    let tuple(elem, inner_acc) = f(elem, inner_acc)
    tuple([elem, ..mapped], inner_acc)
  }

  let tuple(items, acc) =
    list.fold(over: items, from: tuple([], acc), with: reducer)
  tuple(list.reverse(items), acc)
}
