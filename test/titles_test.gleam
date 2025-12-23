import birdie
import birdie/internal/titles
import glance
import gleam/dict
import gleam/list
import gleam/string

const module = "
{{birdie_import}}

pub fn in_pipeline_with_piped_content() {
  some_content
  |> in_a_pipeline
  |> with_some_steps
  |> {{snap_invocation}}(title: \"with label\")

  some_content
  |> in_a_pipeline
  |> with_some_steps
  |> {{snap_invocation}}(\"without label\")
}

pub fn in_pipeline_with_piped_title() {
  \"with function capture and labels\"
  |> {{snap_invocation}}(content: \"content\", title: _)

  \"with function capture and some labels\"
  |> {{snap_invocation}}(\"content\", title: _)

  \"with function capture and no labels\"
  |> {{snap_invocation}}(\"content\", _)

  \"without function capture and labelled content\"
  |> {{snap_invocation}}(content: \"content\")
}

pub fn direct_call() {
  {{snap_invocation}}(\"content\", \"with no labels\")
  {{snap_invocation}}(content: \"content\", title: \"with labels\")
  {{snap_invocation}}(\"content\", title: \"with just title label\")
  {{snap_invocation}}(\"with just content label\", content: \"content\")
  {{snap_invocation}}(title: \"with swapped labels\", content: \"content\")
}
"

//const module_with_prefixes = "
//{{birdie_import}}
//
//pub fn in_pipeline_with_piped_content() {
//  some_content
//  |> in_a_pipeline
//  |> with_some_steps
//  |> {{snap_invocation}}(title: \"in pipeline with label\" <> x)
//
//  some_content
//  |> in_a_pipeline
//  |> with_some_steps
//  |> {{snap_invocation}}(\"without label\" <> x)
//}
//
//pub fn direct_call() {
//  {{snap_invocation}}(\"content\", \"with no labels\" <> x)
//  {{snap_invocation}}(content: \"content\", title: \"with labels\" <> x)
//  {{snap_invocation}}(\"content\", title: \"with just title label\" <> x)
//  {{snap_invocation}}(\"with just content label\" <> x, content: \"content\")
//  {{snap_invocation}}(title: \"with swapped labels\" <> x, content: \"content\")
//}
//"

fn assert_error(module: String) -> titles.Error {
  let assert Ok(module) = glance.module(module)
  let assert Error(error) =
    titles.from_module(titles.new(), "my/module", module)
  error
}

fn assert_titles(module: String) -> titles.Titles {
  let assert Ok(module) = glance.module(module)
  let assert Ok(titles) = titles.from_module(titles.new(), "my/module", module)
  titles
}

fn pretty_titles(ts: titles.Titles) -> String {
  let pretty = fn(title, info) {
    let titles.TestInfo(file: file, test_name: test_name) = info
    let title = string.pad_end(title, to: 40, with: " ")
    let info = "[" <> test_name <> " - " <> file <> "]"
    title <> " " <> info
  }

  let literals =
    dict.to_list(titles.literals(ts))
    |> list.map(fn(pair) { pretty(pair.0, pair.1) })
    |> list.sort(string.compare)
    |> string.join(with: "\n")

  let prefixes =
    dict.to_list(titles.prefixes(ts))
    |> list.map(fn(pair) { pretty(pair.0, pair.1) })
    |> string.join(with: "\n")

  "--- LITERALS ---\n" <> literals <> "\n\n--- PREFIXES ---\n" <> prefixes
}

pub fn can_find_literal_titles_when_calling_birdie_snap_test() {
  module
  |> string.replace(each: "{{birdie_import}}", with: "import birdie")
  |> string.replace(each: "{{snap_invocation}}", with: "birdie.snap")
  |> assert_titles
  |> pretty_titles
  |> birdie.snap(title: "can find literal titles when calling `birdie.snap`")
}

pub fn can_find_literal_titles_when_calling_snap_test() {
  module
  |> string.replace(each: "{{birdie_import}}", with: "import birdie.{snap}")
  |> string.replace(each: "{{snap_invocation}}", with: "snap")
  |> assert_titles
  |> pretty_titles
  |> birdie.snap(title: "can find literal titles when calling `snap`")
}

pub fn can_find_literal_titles_when_calling_aliased_birdie_snap_test() {
  module
  |> string.replace(each: "{{birdie_import}}", with: "import birdie as b")
  |> string.replace(each: "{{snap_invocation}}", with: "b.snap")
  |> assert_titles
  |> pretty_titles
  |> birdie.snap(title: "can find literal titles when calling aliased `b.snap`")
}

pub fn can_find_literal_titles_when_calling_aliased_snap_test() {
  module
  |> string.replace(
    each: "{{birdie_import}}",
    with: "import birdie.{snap as s}",
  )
  |> string.replace(each: "{{snap_invocation}}", with: "s")
  |> assert_titles
  |> pretty_titles
  |> birdie.snap(title: "can find literal titles when calling aliased `s`")
}

pub fn can_find_literal_titles_when_calling_discarded_module_test() {
  module
  |> string.replace(
    each: "{{birdie_import}}",
    with: "import birdie.{snap} as _",
  )
  |> string.replace(each: "{{snap_invocation}}", with: "snap")
  |> assert_titles
  |> pretty_titles
  |> birdie.snap(title: "can find literal titles when calling discarded module")
}

pub fn can_find_literal_titles_when_calling_aliased_discarded_module_test() {
  module
  |> string.replace(
    each: "{{birdie_import}}",
    with: "import birdie.{snap as s} as _",
  )
  |> string.replace(each: "{{snap_invocation}}", with: "s")
  |> assert_titles
  |> pretty_titles
  |> birdie.snap(
    title: "can find literal titles when calling aliased discarded module",
  )
}

// pub fn can_find_prefix_titles_when_calling_birdie_snap_test() {
//   module_with_prefixes
//   |> string.replace(each: "{{birdie_import}}", with: "import birdie")
//   |> string.replace(each: "{{snap_invocation}}", with: "birdie.snap")
//   |> assert_titles
//   |> pretty_titles
//   |> birdie.snap(title: "can find prefix titles when calling `birdie.snap`")
// }
//
// pub fn can_find_prefix_titles_when_calling_snap_test() {
//   module_with_prefixes
//   |> string.replace(each: "{{birdie_import}}", with: "import birdie.{snap}")
//   |> string.replace(each: "{{snap_invocation}}", with: "snap")
//   |> assert_titles
//   |> pretty_titles
//   |> birdie.snap(title: "can find prexif titles when calling `snap`")
// }
//
// pub fn can_find_prefix_titles_when_calling_aliased_birdie_snap_test() {
//   module_with_prefixes
//   |> string.replace(each: "{{birdie_import}}", with: "import birdie as b")
//   |> string.replace(each: "{{snap_invocation}}", with: "b.snap")
//   |> assert_titles
//   |> pretty_titles
//   |> birdie.snap(title: "can find prefix titles when calling aliased `b.snap`")
// }
//
// pub fn can_find_prefix_titles_when_calling_aliased_snap_test() {
//   module_with_prefixes
//   |> string.replace(
//     each: "{{birdie_import}}",
//     with: "import birdie.{snap as s}",
//   )
//   |> string.replace(each: "{{snap_invocation}}", with: "s")
//   |> assert_titles
//   |> pretty_titles
//   |> birdie.snap(title: "can find prefix titles when calling aliased `s`")
// }

pub fn we_get_an_error_for_same_literal_titles_test() {
  let duplicate_title_errors =
    assert_error(
      "
import birdie
pub fn wibble_test() {
  birdie.snap(title: \"wibble\", content: \"\")
  birdie.snap(title: \"wibble\", content: \"\")
}",
    )

  assert duplicate_title_errors
    == titles.DuplicateLiteralTitles(
      title: "wibble",
      one: titles.TestInfo(file: "my/module", test_name: "wibble_test"),
      other: titles.TestInfo(file: "my/module", test_name: "wibble_test"),
    )
}

pub fn can_read_the_snap_titles_from_the_project_itself_test() {
  let assert Ok(titles) = titles.from_test_directory()

  pretty_titles(titles)
  |> birdie.snap(title: "can read the snap titles from the project itself")
}
