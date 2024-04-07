import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub type DiffLine {
  DiffLine(number: Int, line: String, kind: DiffLineKind)
}

pub type DiffLineKind {
  Old
  New
  Shared
}

pub fn histogram(one, other) {
  let one_lines = string.split(one, on: "\n")
  let other_lines = string.split(other, on: "\n")
  let lcs = lcs(one_lines, other_lines)
  match_diff_lines([], lcs, 1, one_lines, 1, other_lines)
}

fn match_diff_lines(
  lines: List(DiffLine),
  lcs: List(String),
  line_one: Int,
  one: List(String),
  line_other: Int,
  other: List(String),
) -> List(DiffLine) {
  case lcs, one, other {
    // We drained all the lines, we can return the accumulator which was built
    // in reverse order.
    [], [], [] -> list.reverse(lines)

    // If we don't have any more lines in the common prefix we first drain all
    // the lines from the first list marking those as old.
    [], [first, ..one], other ->
      [DiffLine(line_one, first, Old), ..lines]
      |> match_diff_lines(lcs, line_one + 1, one, line_other, other)

    // If we've also drained the first list we finish by draining the other one
    // marking all its lines as new ones.
    [], [], [first, ..other] ->
      [DiffLine(line_other, first, New), ..lines]
      |> match_diff_lines(lcs, line_one, one, line_other + 1, other)

    // While the first list has lines that are not in common we add those
    // marking them as old.
    [first_common, ..], [first_one, ..one], other if first_common != first_one ->
      [DiffLine(line_one, first_one, Old), ..lines]
      |> match_diff_lines(lcs, line_one + 1, one, line_other, other)

    // While the second list has lines that are not in common we add those
    // marking them as new.
    [first_common, ..], one, [first_other, ..other] if first_common
      != first_other ->
      [DiffLine(line_other, first_other, New), ..lines]
      |> match_diff_lines(lcs, line_one, one, line_other + 1, other)

    [first_common, ..lcs], [_, ..one], [_, ..other] ->
      [DiffLine(line_other, first_common, Shared), ..lines]
      |> match_diff_lines(lcs, line_one + 1, one, line_other + 1, other)

    [_, ..], [], _ | [_, ..], _, [] -> panic as "unreachable"
  }
}

/// Find the least common subsequences of shared items between two lists.
///
/// Reference: https://tiarkrompf.github.io/notes/?/diff-algorithm/
///
fn lcs(one: List(a), other: List(a)) -> List(a) {
  // The recursive definition is so intuitive and elegant, just please don't
  // look at how `lowest_occurrence_common_item` is defined.
  //
  // 1. We remove the common prefix and suffix from the lists.
  // 2. In the remaining lists we find the common element that appears in both
  //    the list number of times.
  // 3. We recursively look for the `lcs` of the pieces that come before the
  //    common element in both lists and the pieces that come after the common
  //    element in both lists.
  // 4. We join the common prefix and suffix, `lcs`s and common item.

  let #(prefix, one, other) = pop_common_prefix(between: one, and: other)
  let #(suffix, one, other) = pop_common_suffix(between: one, and: other)

  // 💡 A possible optimisation could be using a cache and hit that before
  // calling this function. That might make things faster as well.
  case lowest_occurrence_common_item(one, other) {
    None -> list.concat([prefix, suffix])
    Some(#(item, _, before_a, after_a, before_b, after_b)) ->
      // 💡 A possible optimisation I want to look into is using bags (super
      // fast append only) and turn that into a list only after everything is
      // done. That way we could avoid always repeatedly appending lists.
      list.concat([
        prefix,
        lcs(list.reverse(before_a), list.reverse(before_b)),
        [item],
        lcs(after_a, after_b),
        suffix,
      ])
  }
}

// --- HISTOGRAM ---------------------------------------------------------------

type Occurs(a) {
  One(times: Int, before: List(a), after: List(a))
  Other(times: Int, before: List(a), after: List(a))
  Both(
    times: Int,
    before_one: List(a),
    after_one: List(a),
    before_other: List(a),
    after_other: List(a),
  )
}

fn lowest_occurrence_common_item(
  between one: List(a),
  and other: List(a),
) -> Option(#(a, Int, List(a), List(a), List(a), List(a))) {
  let histogram =
    histogram_add(to: dict.new(), from: one, with: One, acc: [])
    |> histogram_add(from: other, with: Other, acc: [])

  use lowest, a, occurs <- dict.fold(over: histogram, from: None)
  case occurs {
    // We're only looking for items that appear in both.
    One(..) | Other(..) -> lowest
    Both(n, before_one, after_one, before_other, after_other) ->
      case lowest {
        None -> Some(#(a, n, before_one, after_one, before_other, after_other))
        // We keep the one that appears the least, so we compare `n` and `m`,
        // that is the number of occurrences of the current lowest and the new
        // item.
        Some(#(_, m, _, _, _, _)) ->
          case m <= n {
            True -> lowest
            False ->
              #(a, n, before_one, after_one, before_other, after_other)
              |> Some
          }
      }
  }
}

fn histogram_add(
  to histogram: Dict(a, Occurs(a)),
  from list: List(a),
  with to_occurrence: fn(Int, List(a), List(a)) -> Occurs(a),
  acc reverse_prefix: List(a),
) -> Dict(a, Occurs(a)) {
  case list {
    [] -> histogram
    [first, ..rest] ->
      {
        use previous <- dict.update(in: histogram, update: first)
        let new_occurrence = to_occurrence(1, reverse_prefix, rest)
        case previous {
          Some(occurrence) -> sum_occurrences(occurrence, new_occurrence)
          None -> new_occurrence
        }
      }
      |> histogram_add(rest, to_occurrence, [first, ..reverse_prefix])
  }
}

// This is not general purpose and only takes into accounts the particular cases
// that might occur in the histogram building. In particular we first add all
// the `One`s and then all the `Other`s.
fn sum_occurrences(one: Occurs(a), other: Occurs(a)) -> Occurs(a) {
  case one, other {
    One(n, _, _), One(m, before, after) -> One(n + m, before, after)
    Other(n, _, _), Other(m, before, after) -> Other(n + m, before, after)

    One(n, before_one, after_one), Other(m, before_other, after_other)
    | Both(n, before_one, after_one, _, _), Other(m, before_other, after_other) ->
      Both(n + m, before_one, after_one, before_other, after_other)

    _, _ -> panic as "unreachable: sum_occurrences"
  }
}

// --- LIST UTILS --------------------------------------------------------------

/// Returns the common prefix between two lists, and the remaining lists after
/// removing the common prefix from each one.
///
fn pop_common_prefix(
  between one: List(a),
  and other: List(a),
) -> #(List(a), List(a), List(a)) {
  let #(reverse_prefix, one, other) = do_pop_common_prefix([], one, other)
  #(list.reverse(reverse_prefix), one, other)
}

fn do_pop_common_prefix(
  reverse_prefix: List(a),
  one: List(a),
  other: List(a),
) -> #(List(a), List(a), List(a)) {
  case one, other {
    [first_one, ..one], [first_other, ..other] if first_one == first_other ->
      do_pop_common_prefix([first_one, ..reverse_prefix], one, other)
    _, _ -> #(reverse_prefix, one, other)
  }
}

/// Returns the common suffix between two lists, and the remaining lists after
/// removing the common suffix from each one.
///
fn pop_common_suffix(
  between one: List(a),
  and other: List(a),
) -> #(List(a), List(a), List(a)) {
  let #(suffix, reverse_one, reverse_other) =
    do_pop_common_prefix([], list.reverse(one), list.reverse(other))
  #(suffix, list.reverse(reverse_one), list.reverse(reverse_other))
}
