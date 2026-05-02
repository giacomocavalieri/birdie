import birdie
import birdie/internal/diagnostic
import glance
import gleam/option

pub fn single_line_diagnostic_with_no_tooltip_text_test() {
  diagnostic.Diagnostic(
    level: diagnostic.Erro,
    title: "example",
    label: option.Some(diagnostic.Label(
      file_name: "wibble.src",
      source: "highlighted-not-highlighted\nnot shown",
      position: glance.Span(0, 11),
      content: "",
      secondary_label: option.None,
    )),
    text: "",
    hint: option.None,
  )
  |> diagnostic.to_string
  |> birdie.snap(title: "single line diagnostic with no tooltip text")
}

pub fn single_line_diagnostic_with_tooltip_text_test() {
  diagnostic.Diagnostic(
    level: diagnostic.Erro,
    title: "example",
    label: option.Some(diagnostic.Label(
      file_name: "wibble.src",
      source: "highlighted-not-highlighted\nnot shown",
      position: glance.Span(0, 11),
      content: "not good",
      secondary_label: option.None,
    )),
    text: "",
    hint: option.None,
  )
  |> diagnostic.to_string
  |> birdie.snap(title: "single line diagnostic with tooltip text")
}

pub fn single_line_diagnostic_with_secondary_label_right_above_test() {
  diagnostic.Diagnostic(
    level: diagnostic.Erro,
    title: "example",
    label: option.Some(diagnostic.Label(
      file_name: "wibble.src",
      source: "secondary here\nmain",
      position: glance.Span(15, 19),
      content: "primary",
      secondary_label: option.Some(#(glance.Span(10, 14), "")),
    )),
    text: "",
    hint: option.None,
  )
  |> diagnostic.to_string
  |> birdie.snap(
    title: "single line diagnostic with secondary label right above",
  )
}

pub fn single_line_diagnostic_with_secondary_label_right_below_test() {
  diagnostic.Diagnostic(
    level: diagnostic.Warn,
    title: "example",
    label: option.Some(diagnostic.Label(
      file_name: "wibble.src",
      source: "main\nsecondary here",
      position: glance.Span(0, 4),
      content: "primary",
      secondary_label: option.Some(#(glance.Span(15, 19), "secondary")),
    )),
    text: "",
    hint: option.None,
  )
  |> diagnostic.to_string
  |> birdie.snap(
    title: "single line diagnostic with secondary label right below",
  )
}

pub fn single_line_diagnostic_with_secondary_label_below_test() {
  diagnostic.Diagnostic(
    level: diagnostic.Erro,
    title: "example",
    label: option.Some(diagnostic.Label(
      file_name: "wibble.src",
      source: "main\nnot shown\nsecondary here",
      position: glance.Span(0, 4),
      content: "primary",
      secondary_label: option.Some(#(glance.Span(25, 29), "secondary")),
    )),
    text: "",
    hint: option.None,
  )
  |> diagnostic.to_string
  |> birdie.snap(title: "single line diagnostic with secondary label below")
}

pub fn single_line_diagnostic_with_secondary_label_above_test() {
  diagnostic.Diagnostic(
    level: diagnostic.Erro,
    title: "example",
    label: option.Some(diagnostic.Label(
      file_name: "wibble.src",
      source: "secondary here\nnot shown\nmain",
      position: glance.Span(25, 29),
      content: "",
      secondary_label: option.Some(#(glance.Span(10, 14), "")),
    )),
    text: "",
    hint: option.None,
  )
  |> diagnostic.to_string
  |> birdie.snap(title: "single line diagnostic with secondary label above")
}

pub fn single_line_diagnostic_with_secondary_label_on_same_line_test() {
  diagnostic.Diagnostic(
    level: diagnostic.Erro,
    title: "example",
    label: option.Some(diagnostic.Label(
      file_name: "wibble.src",
      source: "secondary primary",
      position: glance.Span(10, 17),
      content: "shown",
      secondary_label: option.Some(#(glance.Span(0, 9), "not shows")),
    )),
    text: "",
    hint: option.None,
  )
  |> diagnostic.to_string
  |> birdie.snap(
    title: "single line diagnostic with secondary label on same line, secondary label is ignored",
  )
}

pub fn multi_line_diagnostic_test() {
  diagnostic.Diagnostic(
    level: diagnostic.Erro,
    title: "example",
    label: option.Some(diagnostic.Label(
      file_name: "wibble.src",
      source: "line here\nhere",
      position: glance.Span(5, 14),
      content: "message",
      secondary_label: option.None,
    )),
    text: "",
    hint: option.None,
  )
  |> diagnostic.to_string
  |> birdie.snap(title: "multi line diagnostic")
}
