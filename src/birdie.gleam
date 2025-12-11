import argv
import birdie/internal/cli.{
  type Command, Accept, CheckStale, DeleteStale, FullCommand, Help,
  MissingSubcommand, Reject, Review, Stale, UnexpectedArgument, UnknownCommand,
  UnknownOption, UnknownSubcommand, WithHelpOption,
}
import envoy

import birdie/internal/diff.{type DiffLine, DiffLine}
import birdie/internal/project
import birdie/internal/titles
import birdie/internal/version
import filepath
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/set
import gleam/string
import gleam_community/ansi
import global_value
import justin
import rank
import simplifile.{Eexist, Enoent}
import term_size

const birdie_version = "1.5.1"

const hint_review_message = "run `gleam run -m birdie` to review the snapshots"

const accepted_extension = "accepted"

const new_extension = "new"

type Error {
  SnapshotWithEmptyTitle

  CannotCreateSnapshotsFolder(reason: simplifile.FileError)

  CannotReadAcceptedSnapshot(reason: simplifile.FileError, source: String)

  CannotReadNewSnapshot(reason: simplifile.FileError, source: String)

  CannotSaveNewSnapshot(
    reason: simplifile.FileError,
    title: String,
    destination: String,
  )

  CannotReadSnapshots(reason: simplifile.FileError, folder: String)

  CannotRejectSnapshot(reason: simplifile.FileError, snapshot: String)

  CannotAcceptSnapshot(reason: simplifile.FileError, snapshot: String)

  CannotReadUserInput

  CorruptedSnapshot(source: String)

  CannotFindProjectRoot(reason: simplifile.FileError)

  CannotCreateReferencedFile(reason: simplifile.FileError)

  CannotReadReferencedFile(reason: simplifile.FileError)

  CannotMarkSnapshotAsReferenced(reason: simplifile.FileError)

  StaleSnapshotsFound(stale_snapshots: List(String))

  CannotDeleteStaleSnapshot(reason: simplifile.FileError)

  MissingReferencedFile

  CannotGetTitles(reason: titles.Error)
}

// --- THE SNAPSHOT TYPE -------------------------------------------------------

type New

type Accepted

type Snapshot(status) {
  Snapshot(title: String, content: String, info: Option(titles.TestInfo))
}

// --- SNAP --------------------------------------------------------------------

/// Returns the path to the referenced file, initialising it to be empty only
/// the first time this function is called.
///
fn global_referenced_file() -> Result(String, Error) {
  use <- global_value.create_with_unique_name("birdie.referenced_file")
  use referenced_file <- result.try(referenced_file_path())
  case simplifile.create_file(referenced_file) {
    Ok(_) -> Ok(referenced_file)
    Error(Eexist) ->
      simplifile.write("", to: referenced_file)
      |> result.replace(referenced_file)
      |> result.map_error(CannotCreateReferencedFile(reason: _))
    Error(reason) -> Error(CannotCreateReferencedFile(reason:))
  }
}

fn referenced_file_path() -> Result(String, Error) {
  use name <- result.try(
    project.name()
    |> result.map_error(CannotReadReferencedFile),
  )
  Ok(filepath.join(get_temp_directory(), name <> "_referenced.txt"))
}

fn get_temp_directory() -> String {
  let temp = {
    use <- result.lazy_or(envoy.get("TMPDIR"))
    use <- result.lazy_or(envoy.get("TEMP"))
    envoy.get("TMP")
  }

  case temp {
    Ok(temp) -> temp
    Error(_) ->
      case is_windows() {
        True -> "C:\\TMP"
        False -> "/tmp"
      }
  }
}

@external(erlang, "birdie_ffi", "is_windows")
fn is_windows() -> Bool

/// Finds the snapshots folder at the root of the project the command is run
/// into. If it's not present the folder is created automatically.
///
fn snapshot_folder() -> Result(String, Error) {
  use <- global_value.create_with_unique_name("birdie.snapshot_folder")
  let result = result.map_error(project.find_root(), CannotFindProjectRoot)
  use project_root <- result.try(result)
  let snapshots_folder = filepath.join(project_root, "birdie_snapshots")

  case simplifile.create_directory(snapshots_folder) {
    Ok(Nil) | Error(Eexist) -> Ok(snapshots_folder)
    Error(error) -> Error(CannotCreateSnapshotsFolder(error))
  }
}

/// Performs a snapshot test with the given title, saving the content to a new
/// snapshot file. All your snapshots will be stored in a folder called
/// `birdie_snapshots` in the project's root.
///
/// The test will fail if there already is an accepted snapshot with the same
/// title and a different content.
/// The test will also fail if there's no accepted snapshot with the same title
/// to make sure you will review new snapshots as well.
///
/// > 🚨 A snapshot is saved to a file named after its title, so all titles
/// > should be unique! Otherwise you'd end up comparing unrelated snapshots.
///
/// > 🐦‍⬛ To review all your snapshots interactively you can run
/// > `gleam run -m birdie`.
/// >
/// > To get an help text and all the available options you can run
/// > `gleam run -m birdie help`.
///
pub fn snap(content content: String, title title: String) -> Nil {
  case do_snap(content, title) {
    Ok(Same) -> Nil

    Ok(NewSnapshotCreated(snapshot, destination: _)) -> {
      let hint_message = ansi.yellow(hint_review_message)
      let hint = InfoLineWithTitle(hint_message, DoNotSplit, "hint")
      let box = new_snapshot_box(snapshot, [hint])

      io.println_error("\n\n" <> box <> "\n")
      panic as "Birdie snapshot test failed"
    }

    Ok(Different(accepted, new)) -> {
      let hint_message = ansi.yellow(hint_review_message)
      let hint = InfoLineWithTitle(hint_message, DoNotSplit, "hint")
      let box = diff_snapshot_box(accepted, new, [hint])

      io.println_error("\n\n" <> box <> "\n")
      panic as "Birdie snapshot test failed"
    }

    Error(error) -> {
      let panic_message = "Birdie snapshot test failed\n" <> explain(error)
      panic as panic_message
    }
  }
}

type Outcome {
  NewSnapshotCreated(snapshot: Snapshot(New), destination: String)
  Different(accepted: Snapshot(Accepted), new: Snapshot(New))
  Same
}

fn do_snap(content: String, title: String) -> Result(Outcome, Error) {
  use _ <- result.try(validate_snapshot_title(title))

  // We have to find the snapshot folder since the `gleam test` command might
  // be run from any subfolder we can't just assume we're in the project's root.
  use folder <- result.try(snapshot_folder())

  // 🚨 When snapping with the `snap` function we don't try and get the test
  // info from the file it's defined in. That would require re-parsing the test
  // directory every single time the `snap` function is called. We just put the
  // `info` field to `None`.
  //
  // That additional data will be retrieved and updated during the review
  // process where the parsing of the test directory can be done just once for
  // all the tests.
  //
  // 💡 TODO: I could investigate using a shared cache or something but it
  //          sounds like a pain to implement and should have to work for both
  //          targets.
  let new = Snapshot(title:, content:, info: None)
  let new_snapshot_path = new_destination(new, folder)
  let accepted_snapshot_path = to_accepted_path(new_snapshot_path)

  // Find an accepted snapshot with the same title to make a comparison.
  use accepted <- result.try(read_accepted(accepted_snapshot_path))
  case accepted {
    // If there's no accepted snapshot then we save the new one as there's no
    // comparison to be made.
    None -> {
      use _ <- result.try(save(new, to: new_snapshot_path))
      Ok(NewSnapshotCreated(snapshot: new, destination: new_snapshot_path))
    }

    // If there's a corresponding accepted snapshot we compare it with the new
    // one.
    Some(accepted) -> {
      // Whenever we find an existing accepted snapshot file, we record that it
      // has been referenced in the current run. So we know it will not be
      // stale!
      use referenced_file <- result.try(global_referenced_file())
      use _ <- result.try(
        simplifile.append(
          filepath.base_name(accepted_snapshot_path) <> "\n",
          to: referenced_file,
        )
        |> result.map_error(CannotMarkSnapshotAsReferenced),
      )

      case accepted.content == new.content {
        True -> {
          // If the file is ok we make sure to delete any lingering `.new` file
          // that might have been leftover from somewhere else.
          let _ = simplifile.delete(new_snapshot_path)
          Ok(Same)
        }

        False -> {
          // If the new snapshot is the same as the old one then there's no need
          // to save it in a `.new` file: we can just say they are the same.
          use _ <- result.try(save(new, to: new_snapshot_path))
          Ok(Different(accepted, new))
        }
      }
    }
  }
}

fn validate_snapshot_title(title: String) -> Result(Nil, Error) {
  case string.trim(title) {
    "" -> Error(SnapshotWithEmptyTitle)
    _ -> Ok(Nil)
  }
}

// --- SNAPSHOT CONTENT DIFFING ------------------------------------------------

fn to_diff_lines(
  accepted: Snapshot(Accepted),
  new: Snapshot(New),
) -> List(DiffLine) {
  let Snapshot(title: _, content: accepted_content, info: _) = accepted
  let Snapshot(title: _, content: new_content, info: _) = new
  diff.histogram(accepted_content, new_content)
}

// --- SNAPSHOT (DE)SERIALISATION ----------------------------------------------

fn split_n(
  string,
  times n: Int,
  on separator: String,
) -> Result(#(List(String), String), Nil) {
  case n <= 0 {
    True -> Ok(#([], string))
    False -> {
      use #(line, rest) <- result.try(string.split_once(string, on: separator))
      use #(lines, rest) <- result.try(split_n(rest, n - 1, separator))
      Ok(#([line, ..lines], rest))
    }
  }
}

fn deserialise(raw: String) -> Result(Snapshot(a), Nil) {
  case split_n(raw, 4, "\n") {
    Ok(#(["---", "version: " <> version, "title: " <> title, "---"], content))
    | Ok(#(
        ["---\r", "version: " <> version, "title: " <> title, "---\r"],
        content,
      )) ->
      Ok(Snapshot(
        title: string.trim(title),
        content: trim_content(content, based_on: version),
        info: None,
      ))

    Ok(_) | Error(_) ->
      case split_n(raw, 6, "\n") {
        Ok(#(
          [
            "---",
            "version: " <> version,
            "title: " <> title,
            "file: " <> file,
            "test_name: " <> test_name,
            "---",
          ],
          content,
        ))
        | Ok(#(
            [
              "---\r",
              "version: " <> version,
              "title: " <> title,
              "file: " <> file,
              "test_name: " <> test_name,
              "---\r",
            ],
            content,
          )) ->
          Ok(Snapshot(
            title: string.trim(title),
            content: trim_content(content, based_on: version),
            info: Some(titles.TestInfo(
              file: string.trim(file),
              test_name: string.trim(test_name),
            )),
          ))

        Ok(_) | Error(_) -> Error(Nil)
      }
  }
}

/// Birdie started adding newlines to the end of files starting from `1.4.0`,
/// so if we're reading a snapshot created from `1.4.0` onwards then we want to
/// make sure to remove that newline!
///
fn trim_content(content: String, based_on version: String) -> String {
  let assert Ok(version) = version.parse(version) as "corrupt birdie version"
  case version.compare(version, version.new(1, 4, 0)) {
    order.Gt | order.Eq -> trim_end_once(content, "\n")
    order.Lt -> content
  }
}

fn trim_end_once(string: String, substring: String) {
  case string.ends_with(string, substring) {
    True -> string.drop_end(string, string.length(substring))
    False -> string
  }
}

fn serialise(snapshot: Snapshot(New)) -> String {
  let Snapshot(title:, content:, info:) = snapshot
  let info_lines = case info {
    None -> []
    Some(titles.TestInfo(file:, test_name:)) -> [
      "file: " <> file,
      "test_name: " <> test_name,
    ]
  }

  [
    [
      "---",
      "version: " <> birdie_version,
      // We escape the newlines in the title so that it fits on one line and it's
      // easier to parse.
      // Is this the best course of action? Probably not.
      // Does this make my life a lot easier? Absolutely! 😁
      "title: " <> string.replace(title, each: "\n", with: "\\n"),
    ],
    info_lines,
    ["---", content],
  ]
  |> list.flatten
  |> string.join(with: "\n")
  // We always add a newline at the end of each snapshot to make sure they're
  // valid files.
  |> string.append("\n")
}

// --- FILE SYSTEM OPERATIONS --------------------------------------------------

/// Save a new snapshot to a given path.
///
fn save(snapshot: Snapshot(New), to destination: String) -> Result(Nil, Error) {
  // Just to make sure I'm not messing up something anywhere else in the code
  // base: a new snapshot's destination MUST always end with a `.new` extension.
  // If it doesn't there's a fatal error in my code and I should fix it.
  case string.ends_with(destination, ".new") {
    False ->
      panic as "Looks like I've messed up something, all new snapshots should have the `.new` extension"

    True ->
      simplifile.write(to: destination, contents: serialise(snapshot))
      |> result.map_error(CannotSaveNewSnapshot(
        reason: _,
        title: snapshot.title,
        destination:,
      ))
  }
}

/// Read an accepted snapshot which might be missing.
///
fn read_accepted(source: String) -> Result(Option(Snapshot(Accepted)), Error) {
  case simplifile.read(source) {
    Ok(content) ->
      case deserialise(content) {
        Ok(snapshot) -> Ok(Some(snapshot))
        Error(Nil) -> Error(CorruptedSnapshot(source))
      }

    Error(Enoent) -> Ok(None)
    Error(reason) -> Error(CannotReadAcceptedSnapshot(reason:, source:))
  }
}

/// Read a new snapshot.
///
/// > ℹ️ Notice the different return type compared to `read_accepted`: when we
/// > try to read a new snapshot we are sure it's there (because we've listed
/// > the directory or something else) so if it's not present that's an error
/// > and we don't return an `Ok(None)`.
///
fn read_new(source: String) -> Result(Snapshot(New), Error) {
  case simplifile.read(source) {
    Ok(content) ->
      result.replace_error(deserialise(content), CorruptedSnapshot(source))
    Error(reason) -> Error(CannotReadNewSnapshot(reason:, source:))
  }
}

/// List all the new snapshots in a folder. Every file is automatically
/// prepended with the folder so you get the full path of each file.
///
fn list_new_snapshots(in folder: String) -> Result(List(String), Error) {
  case simplifile.read_directory(folder) {
    Error(reason) -> Error(CannotReadSnapshots(reason:, folder:))
    Ok(files) ->
      Ok({
        use file <- list.filter_map(files)
        case filepath.extension(file) {
          // Only keep the files with the ".new" extension and join their name
          // with the folder's path.
          Ok(extension) if extension == new_extension ->
            Ok(filepath.join(folder, file))
          _ -> Error(Nil)
        }
      })
  }
}

/// List all the accepted snapshots in a folder. Every file is automatically
/// prepended with the folder so you get the full path of each file.
///
fn list_accepted_snapshots(in folder: String) -> Result(List(String), Error) {
  case simplifile.read_directory(folder) {
    Error(reason) -> Error(CannotReadSnapshots(reason:, folder:))
    Ok(files) ->
      Ok({
        use file <- list.filter_map(files)
        case filepath.extension(file) {
          // Only keep the files with the ".accepted" extension and join their
          // name with the folder's path.
          Ok(extension) if extension == accepted_extension ->
            Ok(filepath.join(folder, file))
          _ -> Error(Nil)
        }
      })
  }
}

fn accept_snapshot(
  new_snapshot_path: String,
  titles: titles.Titles,
) -> Result(Nil, Error) {
  use snapshot <- result.try(read_new(new_snapshot_path))
  let Snapshot(title:, content:, info: _) = snapshot
  let accepted_snapshot_path = to_accepted_path(new_snapshot_path)

  // Once a snapshot is accepted we need to mark it as referenced. Otherwise
  // running `gleam run -m birdie accept` (or `review`) followed by
  // `gleam run -m stale check` would result in all those accepted snapshots
  // being marked as stale!
  use referenced_file <- result.try(referenced_file_path())
  use _ <- result.try(case simplifile.is_file(referenced_file) {
    Ok(_) -> Ok(Nil)
    Error(_) ->
      simplifile.create_file(referenced_file)
      |> result.map_error(CannotCreateReferencedFile)
  })
  use _ <- result.try(
    simplifile.append(
      filepath.base_name(accepted_snapshot_path) <> "\n",
      to: referenced_file,
    )
    |> result.map_error(CannotMarkSnapshotAsReferenced),
  )

  case titles.find(titles, title) {
    // We could find additional info about the test so we add it to the snapshot
    // before saving it! So we delete the `new` file and write an `accepted`
    // one with all the new info we found.
    Ok(titles.Literal(info)) | Ok(titles.Prefix(info, _)) -> {
      let delete_new_snapshot =
        simplifile.delete(new_snapshot_path)
        |> result.map_error(CannotAcceptSnapshot(_, new_snapshot_path))
      use _ <- result.try(delete_new_snapshot)

      Snapshot(title:, content:, info: Some(info))
      |> serialise
      |> simplifile.write(to: accepted_snapshot_path)
      |> result.map_error(CannotAcceptSnapshot(_, accepted_snapshot_path))
    }

    Error(_) ->
      // Birdie couldn't find any additional info about the given test, so
      // we can just move the `new` snapshot to the `accepted` one.
      simplifile.rename(new_snapshot_path, accepted_snapshot_path)
      |> result.map_error(CannotAcceptSnapshot(_, new_snapshot_path))
  }
}

fn reject_snapshot(new_snapshot_path: String) -> Result(Nil, Error) {
  simplifile.delete(new_snapshot_path)
  |> result.map_error(CannotRejectSnapshot(_, new_snapshot_path))
}

// --- UTILITIES ---------------------------------------------------------------

/// Turns a snapshot's title into a file name stripping it of all dangerous
/// characters (or at least those I could think ok 😁).
///
fn file_name(title: String) -> String {
  string.replace(each: "/", with: " ", in: title)
  |> string.replace(each: "\\", with: " ")
  |> string.replace(each: "\n", with: " ")
  |> string.replace(each: "\t", with: " ")
  |> string.replace(each: "\r", with: " ")
  |> string.replace(each: ".", with: " ")
  |> string.replace(each: ":", with: " ")
  |> justin.snake_case
}

/// Returns the path where a new snapshot should be saved.
///
fn new_destination(snapshot: Snapshot(New), folder: String) -> String {
  filepath.join(folder, file_name(snapshot.title)) <> "." <> new_extension
}

/// Turns a new snapshot path into the path of the corresponding accepted
/// snapshot.
///
fn to_accepted_path(file: String) -> String {
  // This just replaces the `.new` extension with the `.accepted` one.
  filepath.strip_extension(file) <> "." <> accepted_extension
}

// --- PRETTY PRINTING ---------------------------------------------------------

fn explain(error: Error) -> String {
  let heading = fn(reason: simplifile.FileError) {
    "[" <> ansi.bold(string.inspect(reason)) <> "] "
  }

  let message = case error {
    SnapshotWithEmptyTitle ->
      "a snapshot cannot have the empty string as a title."

    CannotCreateSnapshotsFolder(reason:) ->
      heading(reason) <> "I couldn't create the snapshots folder."

    CannotReadAcceptedSnapshot(reason:, source:) ->
      heading(reason)
      <> "I couldn't read the accepted snapshot from "
      <> ansi.italic("\"" <> source <> "\".")

    CannotReadNewSnapshot(reason:, source:) ->
      heading(reason)
      <> "I couldn't read the new snapshot from "
      <> ansi.italic("\"" <> source <> "\".")

    CannotSaveNewSnapshot(reason:, title:, destination:) ->
      heading(reason)
      <> "I couldn't save the snapshot "
      <> ansi.italic("\"" <> title <> "\" ")
      <> "to "
      <> ansi.italic("\"" <> destination <> "\".")

    CannotReadSnapshots(reason:, folder: _) ->
      heading(reason) <> "I couldn't read the snapshots folder's contents."

    CannotRejectSnapshot(reason:, snapshot:) ->
      heading(reason)
      <> "I couldn't reject the snapshot "
      <> ansi.italic("\"" <> snapshot <> "\".")

    CannotAcceptSnapshot(reason:, snapshot:) ->
      heading(reason)
      <> "I couldn't accept the snapshot "
      <> ansi.italic("\"" <> snapshot <> "\".")

    CannotReadUserInput -> "I couldn't read the user input."

    CorruptedSnapshot(source:) ->
      "It looks like "
      <> ansi.italic("\"" <> source <> "\"\n")
      <> "is not a valid snapshot.\n"
      <> "This might happen when someone modifies its content.\n"
      <> "Try deleting the snapshot and recreating it."

    CannotCreateReferencedFile(reason:) ->
      heading(reason)
      <> "I couldn't create the file used to track stale snapshots."

    CannotReadReferencedFile(reason:) ->
      heading(reason)
      <> "I couldn't read the file used to track stale snapshots."

    CannotMarkSnapshotAsReferenced(reason:) ->
      heading(reason)
      <> "I couldn't write to the file used to track stale snapshots."

    CannotFindProjectRoot(reason:)
    | CannotGetTitles(titles.CannotFindProjectRoot(reason:)) ->
      heading(reason)
      <> "I couldn't locate the project's root where the snapshot's"
      <> " folder should be."

    MissingReferencedFile -> {
      "I couldn't find any information about stale snapshots.\n"
      <> "Remember you have to run `gleam test` first, so I can find any stale "
      <> "snapshot."
    }

    StaleSnapshotsFound(stale_snapshots:) -> {
      let titles =
        list.map(stale_snapshots, fn(snapshot) {
          "  - " <> filepath.strip_extension(snapshot)
        })
        |> string.join(with: "\n")

      "I found the following stale snapshots:\n\n"
      <> titles
      <> "\n\n"
      <> "These snapshots were not referenced by any snapshot test during the "
      <> "last `gleam test`\n"
      <> "Hint: run `gleam run -m birdie stale delete` to delete them"
    }

    CannotDeleteStaleSnapshot(reason:) -> {
      heading(reason) <> "I couldn't delete one of the stale snapshots."
    }

    CannotGetTitles(titles.CannotReadTestDirectory(reason:)) ->
      heading(reason) <> "I couldn't list the contents of the test folder."

    CannotGetTitles(titles.CannotReadTestFile(reason:, file:)) ->
      heading(reason)
      <> "I couldn't read the test file "
      <> ansi.italic("\"" <> file <> "\"\n")

    CannotGetTitles(titles.DuplicateLiteralTitles(
      title:,
      one: titles.TestInfo(file: one_file, test_name: one_test_name),
      other: titles.TestInfo(file: other_file, test_name: other_test_name),
    )) -> {
      let same_file = one_file == other_file
      let same_function = one_test_name == other_test_name
      let location = case same_file, same_function {
        True, True ->
          "Both tests are defined in:\n\n  "
          <> ansi.italic(to_function_name(one_file, one_test_name))
        _, _ ->
          "One test is defined in:\n\n  "
          <> ansi.italic(to_function_name(one_file, one_test_name))
          <> "\n\nWhile the other is defined in:\n\n  "
          <> ansi.italic(to_function_name(other_file, other_test_name))
      }

      "it looks like there's some snapshot tests sharing the same title:

  " <> ansi.italic("\"" <> title <> "\"") <> "

Snapshot titles " <> ansi.bold("must be unique") <> " or you would run into strange diffs
when reviewing them, try changing one of those.
" <> location
    }

    CannotGetTitles(titles.OverlappingPrefixes(..)) ->
      panic as "Prefixes are not implemented yet"

    CannotGetTitles(titles.PrefixOverlappingWithLiteralTitle(..)) ->
      panic as "Prefixes are not implemented yet"
  }

  message
}

fn to_function_name(file: String, function_name: String) -> String {
  let module_name = case file {
    "./test/" <> rest -> filepath.strip_extension(rest)
    _ -> filepath.strip_extension(file)
  }

  module_name <> ".{" <> function_name <> "}"
}

type InfoLine {
  InfoLineWithTitle(content: String, split: Split, title: String)
  InfoLineWithNoTitle(content: String, split: Split)
}

type Split {
  DoNotSplit
  SplitWords
  Truncate
}

fn snapshot_default_lines(snapshot: Snapshot(status)) -> List(InfoLine) {
  let Snapshot(title:, content: _, info:) = snapshot
  case info {
    None -> [InfoLineWithTitle(title, SplitWords, "title")]
    Some(titles.TestInfo(file:, test_name:)) -> [
      InfoLineWithTitle(title, SplitWords, "title"),
      InfoLineWithTitle(file, Truncate, "file"),
      InfoLineWithTitle(test_name, Truncate, "name"),
    ]
  }
}

fn new_snapshot_box(
  snapshot: Snapshot(New),
  additional_info_lines: List(InfoLine),
) -> String {
  let Snapshot(title: _, content:, info: _) = snapshot

  let content =
    string.split(content, on: "\n")
    |> list.index_map(fn(line, i) {
      DiffLine(number: i + 1, line:, kind: diff.New)
    })

  pretty_box(
    "new snapshot",
    content,
    list.flatten([snapshot_default_lines(snapshot), additional_info_lines]),
    fn(shared_line) { shared_line },
  )
}

fn diff_snapshot_box(
  accepted: Snapshot(Accepted),
  new: Snapshot(New),
  additional_info_lines: List(InfoLine),
) -> String {
  pretty_box(
    "mismatched snapshots",
    to_diff_lines(accepted, new),
    [
      snapshot_default_lines(accepted),
      additional_info_lines,
      [
        InfoLineWithNoTitle("", DoNotSplit),
        InfoLineWithNoTitle(ansi.red("- old snapshot"), DoNotSplit),
        InfoLineWithNoTitle(ansi.green("+ new snapshot"), DoNotSplit),
      ],
    ]
      |> list.flatten,
    fn(shared_line) { ansi.dim(shared_line) },
  )
}

fn regular_snapshot_box(
  new: Snapshot(New),
  additional_info_lines: List(InfoLine),
) {
  let Snapshot(title: _, content:, info: _) = new

  let content =
    string.split(content, on: "\n")
    |> list.index_map(fn(line, i) {
      DiffLine(number: i + 1, line:, kind: diff.Shared)
    })

  pretty_box(
    "mismatched snapshots",
    content,
    [snapshot_default_lines(new), additional_info_lines]
      |> list.flatten,
    fn(shared_line) { shared_line },
  )
}

fn count_digits(number: Int) -> Int {
  count_digits_loop(int.absolute_value(number), 0)
}

fn count_digits_loop(number: Int, digits: Int) -> Int {
  case number < 10 {
    True -> 1 + digits
    False -> count_digits_loop(number / 10, 1 + digits)
  }
}

fn pretty_box(
  title: String,
  content_lines: List(DiffLine),
  info_lines: List(InfoLine),
  // Determines how a shared diff line is to be displayed
  shared_line_style: fn(String) -> String,
) -> String {
  let width = terminal_width()
  let lines_count = list.length(content_lines) + 1
  let padding = count_digits(lines_count) * 2 + 5

  // Make the title line.
  let title_length = string.length(title)
  let title_line_right = string.repeat("─", width - 5 - title_length)
  let title_line = "── " <> title <> " ─" <> title_line_right

  // Make the pretty info lines.
  let info_lines =
    list.map(info_lines, pretty_info_line(_, width))
    |> string.join("\n")

  // Add numbers to the content's lines.
  let content =
    list.map(content_lines, pretty_diff_line(_, padding, shared_line_style))
    |> string.join(with: "\n")

  // The open and closed delimiters for the box main content.
  let left_padding_line = string.repeat("─", padding)
  let right_padding_line = string.repeat("─", width - padding - 1)
  let open_line = left_padding_line <> "┬" <> right_padding_line
  let closed_line = left_padding_line <> "┴" <> right_padding_line

  // Assemble everything together with some empty lines to allow the content to
  // breath a little.
  [title_line, "", info_lines, "", open_line, content, closed_line]
  |> string.join(with: "\n")
}

fn pretty_info_line(line: InfoLine, width: Int) -> String {
  let #(prefix, prefix_length) = case line {
    InfoLineWithNoTitle(..) -> #("  ", 2)
    InfoLineWithTitle(title:, ..) -> #(
      "  " <> ansi.blue(title <> ": "),
      string.length(title) + 4,
    )
  }

  case line.split {
    Truncate -> prefix <> truncate(line.content, width - prefix_length)
    DoNotSplit -> prefix <> line.content
    SplitWords ->
      case to_lines(line.content, width - prefix_length) {
        [] -> prefix
        [line, ..lines] -> {
          use acc, line <- list.fold(over: lines, from: prefix <> line)
          acc <> "\n" <> string.repeat(" ", prefix_length) <> line
        }
      }
  }
}

fn pretty_diff_line(
  diff_line: DiffLine,
  padding: Int,
  shared_line_style: fn(String) -> String,
) -> String {
  let DiffLine(number:, line:, kind:) = diff_line

  let #(pretty_number, pretty_line, separator) = case kind {
    diff.Shared -> #(
      int.to_string(number)
        |> string.pad_start(to: padding - 1, with: " ")
        |> ansi.dim,
      shared_line_style(line),
      " │ ",
    )

    diff.New -> #(
      int.to_string(number)
        |> string.pad_start(to: padding - 1, with: " ")
        |> ansi.green
        |> ansi.bold,
      ansi.green(line),
      ansi.green(" + "),
    )

    diff.Old -> {
      let number =
        { " " <> int.to_string(number) }
        |> string.pad_end(to: padding - 1, with: " ")
      #(ansi.red(number), ansi.red(line), ansi.red(" - "))
    }
  }

  pretty_number <> separator <> pretty_line
}

// --- STRING UTILITIES --------------------------------------------------------

fn truncate(string: String, max_length: Int) -> String {
  case string.length(string) > max_length {
    False -> string
    True ->
      string.to_graphemes(string)
      |> list.take(max_length - 3)
      |> string.join(with: "")
      |> string.append("...")
  }
}

fn to_lines(string: String, max_length: Int) -> List(String) {
  // We still want to keep the original lines, so we work line by line.
  use line <- list.flat_map(string.split(string, on: "\n"))
  let words = string.split(line, on: " ")
  do_to_lines([], "", 0, words, max_length)
}

fn do_to_lines(
  lines: List(String),
  line: String,
  line_length: Int,
  words: List(String),
  max_length: Int,
) -> List(String) {
  case words {
    [] ->
      case line == "" {
        True -> list.reverse(lines)
        False -> list.reverse([line, ..lines])
      }

    [word, ..rest] -> {
      let word_length = string.length(word)
      let new_line_length = word_length + line_length + 1
      // ^ With the +1 we account for the whitespace that separates words!
      case new_line_length > max_length {
        True -> do_to_lines([line, ..lines], "", 0, words, max_length)
        False -> {
          let new_line = case line {
            "" -> word
            _ -> line <> " " <> word
          }
          do_to_lines(lines, new_line, new_line_length, rest, max_length)
        }
      }
    }
  }
}

// --- CLI COMMAND -------------------------------------------------------------

/// Reviews the snapshots in the project's folder.
/// This function will behave differently depending on the command line
/// arguments provided to the program.
/// To have a look at all the available options you can run
/// `gleam run -m birdie help`.
///
/// > 🐦‍⬛ The recommended workflow is to first run your gleeunit tests with
/// > `gleam test` and then review any new/failing snapshot manually running
/// > `gleam run -m birdie`.
/// >
/// > And don't forget to commit your snapshots! Those should be treated as code
/// > and checked with the vcs you're using.
///
pub fn main() -> Nil {
  parse_and_run(argv.load().arguments)
}

fn parse_and_run(args: List(String)) {
  case cli.parse(args) {
    Ok(command) -> run_command(command)

    Error(UnknownOption(command:, option:)) -> {
      cli.unknown_option_error(birdie_version, command, option)
      |> io.println
      exit(1)
    }
    Error(UnknownSubcommand(command:, subcommand:)) -> {
      cli.unknown_subcommand_error(birdie_version, command, subcommand)
      |> io.println
      exit(1)
    }
    Error(MissingSubcommand(command:)) -> {
      cli.missing_subcommand_error(birdie_version, command)
      |> io.println
      exit(1)
    }
    Error(UnexpectedArgument(command:, argument:)) -> {
      cli.unexpected_argument_error(birdie_version, command, argument)
      |> io.println
      exit(1)
    }
    Error(UnknownCommand(command:)) ->
      case cli.similar_command(to: command) {
        Error(Nil) -> {
          cli.unknown_command_error(command, True)
          |> io.println
          exit(1)
        }

        Ok(new_command) -> {
          cli.unknown_command_error(command, False)
          |> io.println

          let prompt =
            "I think you misspelled `"
            <> new_command
            <> "`, would you like me to run it instead?"

          case ask_yes_or_no(prompt) {
            No -> {
              io.println("\n" <> cli.main_help_text())
              exit(1)
            }
            Yes ->
              replace_first(command, with: new_command, in: args)
              |> parse_and_run
          }
        }
      }
  }
}

fn ask_yes_or_no(prompt: String) -> Answer {
  case get_line(prompt <> " [Y/n] ") {
    Error(_) -> No
    Ok(line) ->
      case string.lowercase(line) |> string.trim {
        "yes" | "y" | "" -> Yes
        _ -> No
      }
  }
}

type Answer {
  Yes
  No
}

fn run_command(command: Command) -> Nil {
  case command {
    Review -> report_status(review())
    Accept -> report_status(accept_all())
    Reject -> report_status(reject_all())
    Stale(CheckStale) -> report_status(check_stale())
    Stale(DeleteStale) -> report_status(delete_stale())

    Help ->
      io.println(cli.help_text(
        birdie_version,
        for: Help,
        explaining: FullCommand,
      ))

    WithHelpOption(command:, explained:) ->
      io.println(cli.help_text(
        birdie_version,
        for: command,
        explaining: explained,
      ))
  }
}

fn review() -> Result(Nil, Error) {
  use snapshots_folder <- result.try(snapshot_folder())
  let get_titles = titles.from_test_directory()
  use titles <- result.try(result.map_error(get_titles, CannotGetTitles))
  use _ <- result.try(update_accepted_snapshots(snapshots_folder, titles))

  // Before reviewing, we want to update the files of all the existing snapshots
  // because they might have been moved to a different module, changing their
  // source `file`.
  use _ <- result.try(do_review(snapshots_folder, titles))
  Ok(Nil)
}

fn update_accepted_snapshots(
  snapshots_folder: String,
  titles: titles.Titles,
) -> Result(Nil, Error) {
  use accepted_snapshots <- result.try(list_accepted_snapshots(snapshots_folder))
  use accepted_snapshot <- list.try_each(accepted_snapshots)
  use snapshot <- result.try(read_accepted(accepted_snapshot))
  case snapshot {
    None -> Ok(Nil)
    Some(Snapshot(title:, content: _, info:) as snapshot) ->
      case titles.find(titles, title), info {
        Ok(match), Some(existing_info) if match.info != existing_info ->
          Snapshot(..snapshot, info: Some(match.info))
          |> serialise
          |> simplifile.write(to: accepted_snapshot)
          |> result.map_error(CannotAcceptSnapshot(_, accepted_snapshot))

        Ok(match), None ->
          Snapshot(..snapshot, info: Some(match.info))
          |> serialise
          |> simplifile.write(to: accepted_snapshot)
          |> result.map_error(CannotAcceptSnapshot(_, accepted_snapshot))

        _, _ -> Ok(Nil)
      }
  }
}

fn do_review(
  snapshots_folder: String,
  titles: titles.Titles,
) -> Result(Nil, Error) {
  use new_snapshots <- result.try(list_new_snapshots(in: snapshots_folder))
  case list.length(new_snapshots) {
    // If there's no snapshots to review, we're done!
    0 -> {
      io.println("No new snapshots to review.")
      Ok(Nil)
    }
    // If there's snapshots to review start the interactive session.
    n -> {
      let result = review_loop(new_snapshots, titles, 1, n, ShowDiff)
      // Despite the review process ending well or with an error, we want to
      // clear the screen of any garbage before showing the error explanation
      // or the happy completion string.
      // That's why we postpone the `result.try` step.
      clear()
      use _ <- result.try(result)
      // A nice message based on the number of snapshots :)
      io.println(case n {
        1 -> "Reviewed one snapshot"
        n -> "Reviewed " <> int.to_string(n) <> " snapshots"
      })
      Ok(Nil)
    }
  }
}

/// Reviews all the new snapshots one by one.
fn review_loop(
  new_snapshot_paths: List(String),
  titles: titles.Titles,
  current: Int,
  out_of: Int,
  mode: ReviewMode,
) -> Result(Nil, Error) {
  case new_snapshot_paths {
    [] -> Ok(Nil)
    [new_snapshot_path, ..rest] -> {
      clear()
      // We try reading the new snapshot and the accepted one (which might be
      // missing).
      use new_snapshot <- result.try(read_new(new_snapshot_path))

      // We need to add to the new test info about its location and the function
      // it's defined in.
      let new_snapshot_info = case titles.find(titles, new_snapshot.title) {
        Ok(titles.Prefix(info:, ..)) | Ok(titles.Literal(info:)) -> Some(info)
        Error(_) -> None
      }
      let new_snapshot = Snapshot(..new_snapshot, info: new_snapshot_info)

      let accepted_snapshot_path = to_accepted_path(new_snapshot_path)
      use accepted_snapshot <- result.try(read_accepted(accepted_snapshot_path))

      let progress =
        ansi.dim("Reviewing ")
        <> ansi.bold(ansi.yellow(rank.ordinalise(current)))
        <> ansi.dim(" out of ")
        <> ansi.bold(ansi.yellow(int.to_string(out_of)))

      // If there's no accepted snapshot then we're just reviewing a new
      // snapshot. Otherwise we show a nice diff.
      let box = case accepted_snapshot, mode {
        None, _ -> new_snapshot_box(new_snapshot, [])
        Some(accepted_snapshot), ShowDiff ->
          diff_snapshot_box(accepted_snapshot, new_snapshot, [])
        Some(_accepted_snapshot), HideDiff ->
          regular_snapshot_box(new_snapshot, [])
      }
      io.println(progress <> "\n\n" <> box <> "\n")

      // We ask the user what to do with this snapshot.
      use choice <- result.try(ask_choice(mode))
      case choice {
        AcceptSnapshot -> {
          use _ <- result.try(accept_snapshot(new_snapshot_path, titles))
          review_loop(rest, titles, current + 1, out_of, mode)
        }
        RejectSnapshot -> {
          use _ <- result.try(reject_snapshot(new_snapshot_path))
          review_loop(rest, titles, current + 1, out_of, mode)
        }
        SkipSnapshot -> {
          review_loop(rest, titles, current + 1, out_of, mode)
        }
        ToggleDiffView -> {
          let mode = toggle_mode(mode)
          review_loop(new_snapshot_paths, titles, current, out_of, mode)
        }
      }
    }
  }
}

/// Wether or not we should be showing a diff during the current review process.
///
type ReviewMode {
  ShowDiff
  HideDiff
}

fn toggle_mode(mode: ReviewMode) -> ReviewMode {
  case mode {
    ShowDiff -> HideDiff
    HideDiff -> ShowDiff
  }
}

/// The choice the user can make when reviewing a snapshot.
///
type ReviewChoice {
  AcceptSnapshot
  RejectSnapshot
  SkipSnapshot
  ToggleDiffView
}

/// Asks the user to make a choice: it first prints a reminder of the options
/// and waits for the user to choose one.
/// Will prompt again if the choice is not amongst the possible options.
///
fn ask_choice(mode: ReviewMode) -> Result(ReviewChoice, Error) {
  let diff_message = case mode {
    HideDiff -> " show diff  "
    ShowDiff -> " hide diff  "
  }

  io.println(
    {
      ansi.bold(ansi.green("  a"))
      <> " accept     "
      <> ansi.dim("accept the new snapshot\n")
    }
    <> {
      ansi.bold(ansi.red("  r"))
      <> " reject     "
      <> ansi.dim("reject the new snapshot\n")
    }
    <> {
      ansi.bold(ansi.yellow("  s"))
      <> " skip       "
      <> ansi.dim("skip the snapshot for now\n")
    }
    <> {
      ansi.bold(ansi.cyan("  d"))
      <> diff_message
      <> ansi.dim("toggle snapshot diff\n")
    },
  )

  // We clear the line of any possible garbage that might still be there from
  // a previous prompt of the same method.
  clear_line()
  case result.map(get_line("> "), string.trim) {
    Ok("a") -> Ok(AcceptSnapshot)
    Ok("r") -> Ok(RejectSnapshot)
    Ok("s") -> Ok(SkipSnapshot)
    Ok("d") -> Ok(ToggleDiffView)
    // If the choice is not one of the proposed ones we move the cursor back to
    // the top of where it was and print everything once again, asking for a
    // valid option.
    Ok(_) -> {
      cursor_up(6)
      ask_choice(mode)
    }
    Error(_) -> Error(CannotReadUserInput)
  }
}

fn accept_all() -> Result(Nil, Error) {
  io.println("Looking for new snapshots...")
  use snapshots_folder <- result.try(snapshot_folder())
  use new_snapshots <- result.try(list_new_snapshots(in: snapshots_folder))

  let get_titles = titles.from_test_directory()
  use titles <- result.try(result.map_error(get_titles, CannotGetTitles))
  use _ <- result.try(update_accepted_snapshots(snapshots_folder, titles))

  case list.length(new_snapshots) {
    0 -> io.println("No new snapshots to accept.")
    1 -> io.println("Accepting one new snapshot.")
    n -> io.println("Accepting " <> int.to_string(n) <> " new snapshots.")
  }

  list.try_each(new_snapshots, accept_snapshot(_, titles))
}

fn reject_all() -> Result(Nil, Error) {
  io.println("Looking for new snapshots...")
  use snapshots_folder <- result.try(snapshot_folder())
  use new_snapshots <- result.try(list_new_snapshots(in: snapshots_folder))

  let get_titles = titles.from_test_directory()
  use titles <- result.try(result.map_error(get_titles, CannotGetTitles))
  use _ <- result.try(update_accepted_snapshots(snapshots_folder, titles))

  case list.length(new_snapshots) {
    0 -> io.println("No new snapshots to reject.")
    1 -> io.println("Rejecting one new snapshot.")
    n -> io.println("Rejecting " <> int.to_string(n) <> " new snapshots.")
  }

  list.try_each(new_snapshots, reject_snapshot)
}

fn stale_snapshots_file_names() -> Result(List(String), Error) {
  use snapshots_folder <- result.try(snapshot_folder())
  use referenced_file <- result.try(referenced_file_path())
  case simplifile.read(referenced_file) {
    // If the file is not there we just give up. It means that we didn't run
    // `gleam test` beforehand.
    Error(Enoent) -> Error(MissingReferencedFile)

    // If the file cannot be read for any other reason we end up reporting the
    // error.
    Error(reason) -> Error(CannotReadReferencedFile(reason:))

    // Otherwise we can continue checking!
    Ok(non_stale_snapshots) -> {
      let existing_accepted_snapshots =
        simplifile.get_files(in: snapshots_folder)
        |> result.unwrap(or: [])
        |> list.fold(from: set.new(), with: fn(files, file) {
          case filepath.extension(file) == Ok(accepted_extension) {
            True -> set.insert(files, filepath.base_name(file))
            False -> files
          }
        })

      let non_stale_snapshots = string.split(non_stale_snapshots, on: "\n")

      existing_accepted_snapshots
      |> set.drop(non_stale_snapshots)
      |> set.to_list
      |> Ok
    }
  }
}

fn check_stale() -> Result(Nil, Error) {
  io.println("Checking stale snapshots...")
  use stale_snapshots <- result.try(stale_snapshots_file_names())
  case stale_snapshots {
    [] -> Ok(Nil)
    [_, ..] -> Error(StaleSnapshotsFound(stale_snapshots:))
  }
}

fn delete_stale() -> Result(Nil, Error) {
  io.println("Checking stale snapshots...")
  use snapshots_folder <- result.try(snapshot_folder())
  use stale_snapshots <- result.try(stale_snapshots_file_names())

  list.try_each(stale_snapshots, fn(stale_snapshot) {
    filepath.join(snapshots_folder, stale_snapshot)
    |> simplifile.delete
  })
  |> result.map_error(CannotDeleteStaleSnapshot(reason: _))
}

fn report_status(result: Result(Nil, Error)) -> Nil {
  case result {
    Ok(Nil) -> {
      io.println(ansi.green("🐦‍⬛ Done!"))
      exit(0)
    }
    Error(error) -> {
      io.println_error(ansi.red("Error: ") <> explain(error))
      exit(1)
    }
  }
}

fn terminal_width() -> Int {
  case term_size.get() {
    Ok(#(_, columns)) -> columns
    Error(_) -> 80
  }
}

// --- HELPERS -----------------------------------------------------------------

/// Replaces the first occurrence of an element in the list with the given
/// replacement.
///
fn replace_first(in list: List(a), item item: a, with replacement: a) -> List(a) {
  case list {
    [] -> []
    [first, ..rest] if first == item -> [replacement, ..rest]
    [first, ..rest] -> [first, ..replace_first(rest, item, replacement)]
  }
}

/// Clear the screen.
///
fn clear() -> Nil {
  io.print("\u{1b}c")
  io.print("\u{1b}[H\u{1b}[J")
}

/// Move the cursor up a given number of lines.
///
fn cursor_up(n: Int) -> Nil {
  io.print("\u{1b}[" <> int.to_string(n) <> "A")
}

/// Clear the line the cursor is currently on.
///
fn clear_line() -> Nil {
  io.print("\u{1b}[2K")
}

// --- FFI ---------------------------------------------------------------------

@external(erlang, "erlang", "halt")
fn exit(status_code: Int) -> Nil

/// Reads a line from standard input with the given prompt.
///
/// # Example
///
/// ```gleam
/// get_line("Language: ")
/// // > Language: <- Gleam
/// // -> Ok("Gleam\n")
/// ```
@external(erlang, "birdie_ffi", "get_line")
fn get_line(prompt prompt: String) -> Result(String, Nil)
