//// A module to deal with the intricacies of the language server position data
//// that uses utf16 encoding for some historical reason.
////

import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/string

/// Represents a position inside a file.
///
pub type Position {
  Position(
    /// The line where the position is.
    line: Int,
    /// The utf16 character offset from the given line.
    /// You might wonder why this strange representation, that's because the
    /// language server protocol uses that!
    character: Int,
  )
}

/// A structure used to translate between utf16 positions and utf8 byte indices
/// in a file.
///
pub opaque type Map {
  Map(
    /// A map from number of line to utf8 byte index where it starts.
    line_starts: Dict(Int, Int),
    length: Int,
    /// A map from character index to utf8 size if the size is greater than 1
    /// byte.
    multi_byte_chars: Dict(Int, CharSize),
  )
}

type CharSize {
  CharSize(utf8_size: Int, utf16_size: Int)
}

pub fn map_from_source(source: String) -> Map {
  let source = <<source:utf8>>
  let line_starts = line_starts_loop(source, 0, 0, dict.from_list([#(0, 0)]))
  let multi_byte_chars = multi_byte_chars_loop(source, 0, dict.new())
  Map(line_starts:, length: bit_array.byte_size(source), multi_byte_chars:)
}

fn multi_byte_chars_loop(
  string: BitArray,
  index: Int,
  acc: Dict(Int, CharSize),
) -> Dict(Int, CharSize) {
  case string {
    <<codepoint:utf8_codepoint, rest:bits>> -> {
      case utf8_codepoint_length(codepoint) {
        1 -> multi_byte_chars_loop(rest, index + 1, acc)
        utf8_size -> {
          let utf16_size = utf16_codepoint_length(codepoint)
          let char_size = CharSize(utf8_size:, utf16_size:)
          let acc = dict.insert(acc, index, char_size)
          multi_byte_chars_loop(rest, index + utf8_size, acc)
        }
      }
    }

    <<>> -> acc
    _ -> panic
  }
}

fn utf8_codepoint_length(codepoint: UtfCodepoint) -> Int {
  case string.utf_codepoint_to_int(codepoint) {
    n if n < 128 -> 1
    n if n < 2048 -> 2
    n if n < 65_536 -> 3
    _ -> 4
  }
}

fn utf16_codepoint_length(codepoint: UtfCodepoint) -> Int {
  case string.utf_codepoint_to_int(codepoint) {
    n if n <= 0xFFFF -> 1
    _ -> 2
  }
}

fn line_starts_loop(
  source: BitArray,
  line: Int,
  byte_index: Int,
  acc: Dict(Int, Int),
) -> Dict(Int, Int) {
  case source {
    <<"\n", rest:bits>> -> {
      let acc = dict.insert(acc, line + 1, byte_index + 1)
      line_starts_loop(rest, line + 1, byte_index + 1, acc)
    }
    <<_, rest:bits>> -> line_starts_loop(rest, line, byte_index + 1, acc)
    <<>> -> acc
    _ -> panic as "non byte aligned string"
  }
}

pub fn to_byte_index(numbers: Map, position: Position) -> Int {
  case dict.get(numbers.line_starts, position.line) {
    Error(_) -> numbers.length
    // We check what's the byte index where the line starts, then we need to
    // check where the column brings us.
    Ok(start_offset) -> {
      // The line starts at the given utf8 offset, now we need to figure out
      // where it ends.
      // We can't just add the column because that's the utf16 index! We need to
      // translate that into the equivalent utf8 byte index.
      add_column_offset(numbers, start_offset, 0, position.character)
    }
  }
}

fn add_column_offset(
  numbers: Map,
  byte_index: Int,
  current_utf16_column: Int,
  wanted_utf16_column: Int,
) {
  case current_utf16_column >= wanted_utf16_column {
    True -> byte_index
    False -> {
      case dict.get(numbers.multi_byte_chars, byte_index) {
        Ok(CharSize(utf8_size:, utf16_size:)) -> {
          add_column_offset(
            numbers,
            byte_index + utf8_size,
            current_utf16_column + utf16_size,
            wanted_utf16_column,
          )
        }
        Error(_) ->
          add_column_offset(
            numbers,
            byte_index + 1,
            current_utf16_column + 1,
            wanted_utf16_column,
          )
      }
    }
  }
}

pub fn from_byte_index(numbers: Map, byte_index: Int) -> Position {
  let #(line, start_index) =
    dict.fold(numbers.line_starts, #(-1, -1), fn(acc, line, index) {
      case byte_index < index {
        // The line starts after the byte, so the byte can't be here!
        True -> acc
        // The line starts before the byte index, so it can be. We need to keep
        // only the smaller line: that's going to be the one the index lies on.
        False ->
          case acc {
            #(-1, _) -> #(line, index)
            #(acc_line, _) if acc_line > line -> acc
            #(_, _) -> #(line, index)
          }
      }
    })

  let character = add_index_offset(numbers, start_index, byte_index, 0)
  Position(line:, character:)
}

fn add_index_offset(
  numbers: Map,
  current_byte_index: Int,
  wanted_byte_index: Int,
  utf16_column: Int,
) -> Int {
  case current_byte_index >= wanted_byte_index {
    True -> utf16_column
    False -> {
      case dict.get(numbers.multi_byte_chars, current_byte_index) {
        Ok(CharSize(utf8_size:, utf16_size:)) -> {
          add_index_offset(
            numbers,
            current_byte_index + utf8_size,
            wanted_byte_index,
            utf16_column + utf16_size,
          )
        }
        Error(_) ->
          add_index_offset(
            numbers,
            current_byte_index + 1,
            wanted_byte_index,
            utf16_column + 1,
          )
      }
    }
  }
}
