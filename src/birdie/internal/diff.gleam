import gleam/bool
import gleam/list
import gleam/string
import gleam_community/ansi
import gap
import gap/styling

pub type DiffLine {
  DiffLine(number: Int, line: String, kind: DiffLineKind)
}

pub type DiffLineKind {
  Old
  New
  Shared
}

pub fn line_by_line(old: String, new: String) -> List(DiffLine) {
  let old_lines = string.split(old, on: "\n")
  let new_lines = string.split(new, on: "\n")
  let #(diffs, rest_old, rest_new, line_number) = {
    use old_line, new_line, line <- map2_index(old_lines, new_lines)
    let equal_lines = old_line == new_line
    let shared_diff_line = DiffLine(line, old_line, Shared)
    use <- bool.guard(when: equal_lines, return: [shared_diff_line])

    let comparison =
      gap.compare_strings(new_line, old_line)
      |> styling.from_comparison()
      |> styling.highlight(ansi.underline, ansi.underline, styling.no_highlight)
      |> styling.to_styled_comparison()

    [
      DiffLine(line, comparison.second, Old),
      DiffLine(line, comparison.first, New),
    ]
  }

  list.concat(diffs)
  |> list.append(
    list.index_map(rest_old, fn(line, i) {
      DiffLine(line_number + i + 1, line, Old)
    }),
  )
  |> list.append(
    list.index_map(rest_new, fn(line, i) {
      DiffLine(line_number + i + 1, line, New)
    }),
  )
}

fn map2_index(
  one: List(a),
  other: List(b),
  with fun: fn(a, b, Int) -> c,
) -> #(List(c), List(a), List(b), Int) {
  do_map2_index(one, other, fun, [], 1)
}

fn do_map2_index(
  one: List(a),
  other: List(b),
  fun: fn(a, b, Int) -> c,
  acc: List(c),
  index: Int,
) -> #(List(c), List(a), List(b), Int) {
  case one, other {
    rest_one, [] -> #(list.reverse(acc), rest_one, [], index)
    [], rest_other -> #(list.reverse(acc), [], rest_other, index)
    [one, ..rest_one], [other, ..rest_other] ->
      [fun(one, other, index), ..acc]
      |> do_map2_index(rest_one, rest_other, fun, _, index + 1)
  }
}
