import glance.{type Span}
import gleam/bit_array
import gleam/int
import gleam/option.{type Option}
import gleam/result
import gleam/string
import gleam_community/ansi

pub type Diagnostic {
  Diagnostic(
    level: Level,
    title: String,
    /// If the diagnostic points to some specific position in a source file this
    /// will be `Some`, with the information needed to display such tooltip.
    label: Option(Label),
    text: String,
    /// Some text displayed after the main text, always preceded by the `Hint:`
    /// label.
    hint: Option(String),
  )
}

pub type Level {
  Warn
  Erro
}

pub type Label {
  Label(
    /// The name of the file where the source comes from.
    file_name: String,
    /// The source code this label points to.
    source: String,
    /// The position in the source code we need to point to.
    position: Span,
    /// The content of the label. If this is not an empty string, the label
    /// will have a tooltip connecting it to the label text.
    content: String,
    /// This is an additional label that can be used to add additional context
    /// and is rendered with a muted color.
    secondary_label: Option(#(Span, String)),
  )
}

pub fn to_string(diagnostic: Diagnostic) {
  let Diagnostic(level:, title:, label:, text:, hint:) = diagnostic
  let text = string.trim(text)

  let heading = case level {
    Warn -> ansi.yellow("warning")
    Erro -> ansi.red("error")
  }

  // We start with the heading line...
  let error = ansi.bold(heading <> ": " <> title)
  // ...followed by the label pointing to some source, if present.
  let error = case label {
    option.None -> error
    option.Some(label) -> error <> "\n" <> label_to_string(level, label)
  }
  // Then we add the error text...
  let error = case string.trim(text) {
    "" -> error
    text -> error <> "\n" <> text
  }
  // ...and finally, if present, we also add the hint!
  let error = case option.map(hint, string.trim) {
    option.Some("") | option.None -> error
    option.Some(hint) -> error <> "\nHint: " <> hint
  }

  error
}

/// Turns a label into a pretty string, pointing to the original source.
fn label_to_string(level: Level, label: Label) -> String {
  let Label(file_name:, source:, position:, content:, secondary_label:) = label
  let colour = case level {
    Warn -> ansi.yellow
    Erro -> ansi.red
  }

  let #(start_line, start_line_number, trimmed_to_start) =
    get_line(source, containing: position.start)
  let #(end_line, end_line_number, trimmed_to_end) =
    get_line(source, containing: position.end)

  let is_single_line = start_line_number == end_line_number
  let required_digits =
    int.to_string(start_line_number)
    |> string.length
    |> int.max(int.to_string(end_line_number) |> string.length)

  let file_name =
    string.repeat(" ", required_digits) <> ansi.dim(" ╭─ ") <> file_name
  let empty_line = string.repeat(" ", required_digits) <> ansi.dim(" │")
  let highlighted_code = case is_single_line {
    True -> {
      let start_line =
        colour_string_between_bytes(
          start_line,
          position.start - trimmed_to_start,
          position.end - trimmed_to_start,
          colour,
        )
      ansi.dim(int.to_string(start_line_number) <> " │ ") <> start_line
    }
    False -> {
      let start_line =
        colour_string_between_bytes(
          start_line,
          position.start - trimmed_to_start,
          string.byte_size(start_line),
          colour,
        )
      let end_line =
        colour_string_between_bytes(
          end_line,
          0,
          position.end - trimmed_to_end,
          colour,
        )

      let start_line =
        ansi.dim(
          string.pad_start(
            int.to_string(start_line_number),
            required_digits,
            " ",
          )
          <> " │ ",
        )
        <> start_line

      let end_line =
        ansi.dim(
          string.pad_start(int.to_string(end_line_number), required_digits, " ")
          <> " │ ",
        )
        <> end_line

      case end_line_number - start_line_number {
        0 | 1 -> start_line <> "\n" <> end_line
        _ -> {
          let dashed_line =
            string.repeat(" ", required_digits) <> ansi.dim(" ╎")
          start_line <> "\n" <> dashed_line <> "\n" <> end_line
        }
      }
    }
  }
  // Now we need to add the tooltip to the highlighted code, if the tooltip has
  // any text!
  let tooltip = case is_single_line {
    True -> {
      empty_line
      <> " "
      <> string.repeat(" ", position.start - trimmed_to_start)
      <> string.repeat(colour("^"), position.end - position.start)
      <> case string.trim(content) {
        "" -> ""
        content -> " " <> colour(content)
      }
    }

    False -> {
      empty_line
      <> " "
      <> string.repeat(colour("^"), string.byte_size(start_line))
      <> case string.trim(content) {
        "" -> ""
        content -> " " <> colour(content)
      }
    }
  }

  let primary_label = highlighted_code <> "\n" <> tooltip
  let labels = case secondary_label {
    option.None -> primary_label
    option.Some(#(span, content)) -> {
      let #(secondary_line, secondary_line_number, dropped_bytes) =
        get_line(source, containing: span.start)
      let secondary_line =
        colour_string_between_bytes(
          secondary_line,
          span.start - dropped_bytes,
          span.end - dropped_bytes,
          ansi.dim,
        )
      let secondary_tooltip =
        empty_line
        <> " "
        <> string.repeat(" ", span.start - dropped_bytes)
        <> string.repeat(ansi.dim("~"), span.end - span.start)
        <> case string.trim(content) {
          "" -> ""
          content -> " " <> ansi.dim(content)
        }

      let secondary_label =
        ansi.dim(
          string.pad_start(
            int.to_string(secondary_line_number),
            required_digits,
            " ",
          )
          <> " │ ",
        )
        <> secondary_line
        <> "\n"
        <> secondary_tooltip
      let dashed_line = string.repeat(" ", required_digits) <> ansi.dim(" ╎")
      case secondary_line_number - start_line_number {
        0 -> primary_label
        1 -> primary_label <> "\n" <> secondary_label
        n if n == -1 -> secondary_label <> "\n" <> primary_label
        n if n < 0 ->
          secondary_label <> "\n" <> dashed_line <> "\n" <> primary_label
        _ -> primary_label <> "\n" <> dashed_line <> "\n" <> secondary_label
      }
    }
  }

  file_name <> "\n" <> empty_line <> "\n" <> labels <> "\n" <> empty_line
}

fn colour_string_between_bytes(
  string: String,
  start: Int,
  end: Int,
  colour: fn(String) -> String,
) -> String {
  let result = case <<string:utf8>> {
    <<
      prefix:size(start)-bytes,
      to_colour:size(end - start)-bytes,
      suffix:bytes,
    >> -> {
      use prefix <- result.try(bit_array.to_string(prefix))
      use to_colour <- result.try(bit_array.to_string(to_colour))
      use suffix <- result.try(bit_array.to_string(suffix))
      Ok(prefix <> colour(to_colour) <> suffix)
    }

    _ -> Error(Nil)
  }

  result.unwrap(result, string)
}

/// This returns
/// - the line in `string` containing the given byte
/// - the number of such line in the source code
/// - the number of bytes that come before such line
///
fn get_line(string: String, containing byte: Int) -> #(String, Int, Int) {
  get_line_loop(string, 1, 0, byte)
}

fn get_line_loop(
  string: String,
  line_number: Int,
  trimmed_bytes: Int,
  byte: Int,
) -> #(String, Int, Int) {
  case string.split_once(string, on: "\n") {
    // This is the last line! We can't keep going any further
    Error(_) -> #(string, line_number, trimmed_bytes)
    Ok(#(line, rest)) ->
      case trimmed_bytes + string.byte_size(line) + 1 {
        // The byte falls inside this line, so we have to return it.
        trimmed if trimmed > byte -> #(line, line_number, trimmed_bytes)
        trimmed -> get_line_loop(rest, line_number + 1, trimmed, byte)
      }
  }
}
