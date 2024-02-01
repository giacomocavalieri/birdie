import gleam/bool
import gleam/erlang
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam_community/ansi
import argv
import birdie/internal/diff.{type DiffLine, DiffLine}
import filepath
import gleeunit/should
import justin
import rank
import simplifile

const birdie_version = "1.0.3"

const birdie_snapshots_folder = "birdie_snapshots"

const birdie_test_failed_message = "🐦‍⬛ Birdie snapshot test failed"

const hint_review_message = "run `gleam run -m birdie` to review the snapshots"

type Error {
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
}

// --- THE SNAPSHOT TYPE -------------------------------------------------------

type New

type Accepted

type Snapshot(status) {
  Snapshot(title: String, content: String)
}

// --- SNAP --------------------------------------------------------------------

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
      io.println(birdie_test_failed_message)
      should.fail()
    }

    Ok(Different(accepted, new)) -> {
      let hint_message = ansi.yellow(hint_review_message)
      let hint = InfoLineWithTitle(hint_message, DoNotSplit, "hint")
      let box = diff_snapshot_box(accepted, new, [hint])

      io.println_error("\n\n" <> box <> "\n")
      io.println(birdie_test_failed_message)
      should.fail()
    }

    Error(error) -> {
      explain(error)
      io.println(birdie_test_failed_message)
      should.fail()
    }
  }
}

type Outcome {
  NewSnapshotCreated(snapshot: Snapshot(New), destination: String)
  Different(accepted: Snapshot(Accepted), new: Snapshot(New))
  Same
}

fn do_snap(content: String, title: String) -> Result(Outcome, Error) {
  // We have to find the snapshot folder since the `gleam test` command might
  // be run from any subfolder we can't just assume we're in the project's root.
  use folder <- result.try(find_snapshots_folder())

  let new = Snapshot(title: title, content: content)
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
      // If the new snapshot is the same as the old one then there's no need to
      // save it in a `.new` file: we can just say they are the same.
      use <- bool.guard(when: accepted.content == new.content, return: Ok(Same))
      use _ <- result.try(save(new, to: new_snapshot_path))
      Ok(Different(accepted, new))
    }
  }
}

// --- SNAPSHOT CONTENT DIFFING ------------------------------------------------

fn to_diff_lines(
  accepted: Snapshot(Accepted),
  new: Snapshot(New),
) -> List(DiffLine) {
  let Snapshot(title: _, content: accepted_content) = accepted
  let Snapshot(title: _, content: new_content) = new
  diff.histogram(accepted_content, new_content)
}

// --- SNAPSHOT (DE)SERIALISATION ----------------------------------------------

fn deserialise(raw: String) -> Result(Snapshot(a), Nil) {
  // Check there's the opening `---`
  use #(open_line, rest) <- result.try(string.split_once(raw, "\n"))
  use <- bool.guard(when: open_line != "---", return: Error(Nil))

  // For now I have no use of the version but it might come in handy in the
  // future if I decide to change the snapshots' metadata's format.
  use #(version_line, rest) <- result.try(string.split_once(rest, "\n"))
  use _version <- result.try(case version_line {
    "version: " <> version -> Ok(version)
    _ -> Error(Nil)
  })

  // Get the title.
  use #(title_line, rest) <- result.try(string.split_once(rest, "\n"))
  use title <- result.try(case title_line {
    // We unescape the newlines
    "title: " <> title -> Ok(string.replace(title, each: "\\n", with: "\n"))
    _ -> Error(Nil)
  })

  // Check there's the closing `---`
  use #(close_line, content) <- result.try(string.split_once(rest, "\n"))
  use <- bool.guard(when: close_line != "---", return: Error(Nil))

  Ok(Snapshot(title: title, content: content))
}

fn serialise(snapshot: Snapshot(New)) -> String {
  let Snapshot(title: title, content: content) = snapshot
  [
    "---",
    "version: " <> birdie_version,
    // We escape the newlines in the title so that it fits on one line and it's
    // easier to parse.
    // Is this the best course of action? Probably not.
    // Does this make my life a lot easier? Absolutely! 😁
    "title: " <> string.replace(title, each: "\n", with: "\\n"),
    "---",
    content,
  ]
  |> string.join(with: "\n")
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
        destination: destination,
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

    Error(simplifile.Enoent) -> Ok(None)
    Error(reason) ->
      Error(CannotReadAcceptedSnapshot(reason: reason, source: source))
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
    Error(reason) ->
      Error(CannotReadNewSnapshot(reason: reason, source: source))
  }
}

/// List all the new snapshots in a folder. Every file is automatically
/// prepended with the folder so you get the full path of each file.
///
fn list_new_snapshots(in folder: String) -> Result(List(String), Error) {
  case simplifile.read_directory(folder) {
    Error(reason) -> Error(CannotReadSnapshots(reason: reason, folder: folder))
    Ok(files) ->
      Ok({
        use file <- list.filter_map(files)
        case filepath.extension(file) {
          // Only keep the files with the ".new" extension and join their name
          // with the folder's path.
          Ok("new") -> Ok(filepath.join(folder, file))
          _ -> Error(Nil)
        }
      })
  }
}

/// Finds the snapshots folder at the root of the project the command is run
/// into. If it's not present the folder is created automatically.
///
fn find_snapshots_folder() -> Result(String, Error) {
  let result = result.map_error(find_project_root("."), CannotFindProjectRoot)
  use project_root <- result.try(result)
  let snapshots_folder = filepath.join(project_root, birdie_snapshots_folder)

  case simplifile.create_directory(snapshots_folder) {
    Ok(Nil) | Error(simplifile.Eexist) -> Ok(snapshots_folder)
    Error(error) -> Error(CannotCreateSnapshotsFolder(error))
  }
}

/// Returns the path to the project's root.
///
/// > ⚠️ This assumes that this is only ever run inside a Gleam's project and
/// > sooner or later it will reach a `gleam.toml` file.
/// > Otherwise this will end up in an infinite loop, I think.
///
fn find_project_root(path: String) -> Result(String, simplifile.FileError) {
  let manifest = filepath.join(path, "gleam.toml")
  case simplifile.verify_is_file(manifest) {
    Ok(True) -> Ok(path)
    Ok(False) -> find_project_root(filepath.join(path, ".."))
    Error(reason) -> Error(reason)
  }
}

fn accept_snapshot(new_snapshot_path: String) -> Result(Nil, Error) {
  let accepted_snapshot_path = to_accepted_path(new_snapshot_path)
  simplifile.rename_file(new_snapshot_path, accepted_snapshot_path)
  |> result.map_error(CannotAcceptSnapshot(_, new_snapshot_path))
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
  filepath.join(folder, file_name(snapshot.title)) <> ".new"
}

/// Strips the extension of a file (if it has one).
///
fn strip_extension(file: String) -> String {
  case filepath.extension(file) {
    Ok(extension) -> string.drop_right(file, string.length(extension) + 1)
    Error(Nil) -> file
  }
}

/// Turns a new snapshot path into the path of the corresponding accepted
/// snapshot.
///
fn to_accepted_path(file: String) -> String {
  // This just replaces the `.new` extension with the `.accepted` one.
  strip_extension(file) <> ".accepted"
}

// --- PRETTY PRINTING ---------------------------------------------------------

fn explain(error: Error) -> Nil {
  let heading = fn(reason) { "[" <> ansi.bold(string.inspect(reason)) <> "] " }
  let message = case error {
    CannotCreateSnapshotsFolder(reason: reason) ->
      heading(reason) <> "I couldn't create the snapshots folder"

    CannotReadAcceptedSnapshot(reason: reason, source: source) ->
      heading(reason)
      <> "I couldn't read the accepted snapshot from "
      <> ansi.italic("\"" <> source <> "\"\n")

    CannotReadNewSnapshot(reason: reason, source: source) ->
      heading(reason)
      <> "I couldn't read the new snapshot from "
      <> ansi.italic("\"" <> source <> "\"\n")

    CannotSaveNewSnapshot(
      reason: reason,
      title: title,
      destination: destination,
    ) ->
      heading(reason)
      <> "I couldn't save the snapshot "
      <> ansi.italic("\"" <> title <> "\" ")
      <> "to "
      <> ansi.italic("\"" <> destination <> "\"\n")

    CannotReadSnapshots(reason: reason, folder: _) ->
      heading(reason) <> "I couldn't read the snapshots directory's contents"

    CannotRejectSnapshot(reason: reason, snapshot: snapshot) ->
      heading(reason)
      <> "I couldn't reject the snapshot"
      <> ansi.italic("\"" <> snapshot <> "\" ")

    CannotAcceptSnapshot(reason: reason, snapshot: snapshot) ->
      heading(reason)
      <> "I couldn't accept the snapshot"
      <> ansi.italic("\"" <> snapshot <> "\" ")

    CannotReadUserInput -> "I couldn't read the user input"

    CorruptedSnapshot(source: source) ->
      "It looks like "
      <> ansi.italic("\"" <> source <> "\"\n")
      <> " is not a valid snapshot.\n"
      <> "This might happen when someone modifies its content.\n"
      <> "Try deleting the snapshot and recreating it."

    CannotFindProjectRoot(reason: reason) ->
      heading(reason)
      <> "I couldn't locate the project's root where the snapshot's"
      <> " folder should be."
  }

  io.println_error("❌ " <> ansi.red(message))
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

fn new_snapshot_box(
  snapshot: Snapshot(New),
  additional_info_lines: List(InfoLine),
) -> String {
  let Snapshot(title: title, content: content) = snapshot

  let content =
    string.split(content, on: "\n")
    |> list.index_map(fn(line, i) {
      DiffLine(number: i + 1, line: line, kind: diff.New)
    })

  pretty_box("new snapshot", content, [
    InfoLineWithTitle(title, SplitWords, "title"),
    ..additional_info_lines
  ])
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
      [InfoLineWithTitle(new.title, SplitWords, "title")],
      additional_info_lines,
      [
        InfoLineWithNoTitle("", DoNotSplit),
        InfoLineWithNoTitle(ansi.red("- old snapshot"), DoNotSplit),
        InfoLineWithNoTitle(ansi.green("+ new snapshot"), DoNotSplit),
      ],
    ]
    |> list.concat,
  )
}

fn pretty_box(
  title: String,
  content_lines: List(DiffLine),
  info_lines: List(InfoLine),
) -> String {
  let width = terminal_width()
  let assert Ok(padding) = {
    let lines_count = list.length(content_lines) + 1
    use digits <- result.try(int.digits(lines_count, 10))
    Ok(list.length(digits) * 2 + 5)
  }

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
    list.map(content_lines, pretty_diff_line(_, padding))
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
  let prefix = case line {
    InfoLineWithNoTitle(..) -> "  "
    InfoLineWithTitle(title: title, ..) -> "  " <> title <> ": "
  }
  let prefix_length = string.length(prefix)
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

fn pretty_diff_line(diff_line: DiffLine, padding: Int) -> String {
  let DiffLine(number: number, line: line, kind: kind) = diff_line

  let #(pretty_number, pretty_line, separator) = case kind {
    diff.Shared -> #(
      int.to_string(number)
      |> string.pad_left(to: padding - 1, with: " ")
      |> ansi.dim,
      ansi.dim(line),
      " │ ",
    )

    diff.New -> #(
      int.to_string(number)
      |> string.pad_left(to: padding - 1, with: " ")
      |> ansi.green
      |> ansi.bold,
      ansi.green(line),
      ansi.green(" + "),
    )

    diff.Old -> {
      let number =
        { " " <> int.to_string(number) }
        |> string.pad_right(to: padding - 1, with: " ")
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
          let new_line = line <> " " <> word
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
  case argv.load().arguments {
    [] | ["review"] -> report_status(review())
    ["accept-all"] | ["accept", "all"] -> report_status(accept_all())
    ["reject-all"] | ["reject", "all"] -> report_status(reject_all())
    ["help"] -> help()
    [subcommand] -> unexpected_subcommand(subcommand)
    subcommands -> more_than_one_command(subcommands)
  }
}

fn review() -> Result(Nil, Error) {
  use snapshots_folder <- result.try(find_snapshots_folder())
  use new_snapshots <- result.try(list_new_snapshots(in: snapshots_folder))
  case list.length(new_snapshots) {
    // If there's no snapshots to review, we're done!
    0 -> {
      io.println("No new snapshots to review.")
      Ok(Nil)
    }
    // If there's snapshots to review start the interactive session.
    n -> {
      let result = do_review(new_snapshots, 1, n)
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

fn do_review(
  new_snapshot_paths: List(String),
  current: Int,
  out_of: Int,
) -> Result(Nil, Error) {
  case new_snapshot_paths {
    [] -> Ok(Nil)
    [new_snapshot_path, ..rest] -> {
      clear()
      // We try reading the new snapshot and the accepted one (which might be
      // missing).
      use new_snapshot <- result.try(read_new(new_snapshot_path))
      let accepted_snapshot_path = to_accepted_path(new_snapshot_path)
      use accepted_snapshot <- result.try(read_accepted(accepted_snapshot_path))

      let progress =
        ansi.dim("Reviewing ")
        <> ansi.bold(ansi.yellow(rank.ordinalise(current)))
        <> ansi.dim(" out of ")
        <> ansi.bold(ansi.yellow(int.to_string(out_of)))

      // If there's no accepted snapshot then we're just reviewing a new
      // snapshot. Otherwise we show a nice diff.
      let box = case accepted_snapshot {
        None -> new_snapshot_box(new_snapshot, [])
        Some(accepted_snapshot) ->
          diff_snapshot_box(accepted_snapshot, new_snapshot, [])
      }
      io.println(progress <> "\n\n" <> box <> "\n")

      // We ask the user what to do with this snapshot.
      use choice <- result.try(ask_choice())
      use _ <- result.try(case choice {
        AcceptSnapshot -> accept_snapshot(new_snapshot_path)
        RejectSnapshot -> reject_snapshot(new_snapshot_path)
        SkipSnapshot -> Ok(Nil)
      })

      // Let's keep going with the remaining snapshots.
      do_review(rest, current + 1, out_of)
    }
  }
}

/// The choice the user can make when reviewing a snapshot.
///
type ReviewChoice {
  AcceptSnapshot
  RejectSnapshot
  SkipSnapshot
}

/// Asks the user to make a choice: it first prints a reminder of the options
/// and waits for the user to choose one.
/// Will prompt again if the choice is not amongst the possible options.
///
fn ask_choice() -> Result(ReviewChoice, Error) {
  io.println(
    ansi.bold(ansi.green("  a"))
      <> " accept  "
      <> ansi.dim("accept the new snapshot\n")
      <> ansi.bold(ansi.red("  r"))
      <> " reject  "
      <> ansi.dim("reject the new snapshot\n")
      <> ansi.bold(ansi.yellow("  s"))
      <> " skip    "
      <> ansi.dim("skip the snapshot for now\n"),
  )
  // We clear the line of any possible garbage that might still be there from
  // a previous prompt of the same method.
  clear_line()
  case result.map(erlang.get_line("> "), string.trim) {
    Ok("a") -> Ok(AcceptSnapshot)
    Ok("r") -> Ok(RejectSnapshot)
    Ok("s") -> Ok(SkipSnapshot)
    // If the choice is not one of the proposed ones we move the cursor back to
    // the top of where it was and print everything once again, asking for a
    // valid option.
    Ok(_) -> {
      cursor_up(5)
      ask_choice()
    }
    Error(_) -> Error(CannotReadUserInput)
  }
}

fn accept_all() -> Result(Nil, Error) {
  io.println("Looking for new snapshots...")
  use snapshots_folder <- result.try(find_snapshots_folder())
  use new_snapshots <- result.try(list_new_snapshots(in: snapshots_folder))

  case list.length(new_snapshots) {
    0 -> io.println("No new snapshots to accept.")
    1 -> io.println("Accepting one new snapshot.")
    n -> io.println("Accepting " <> int.to_string(n) <> " new snapshots.")
  }

  list.try_each(new_snapshots, accept_snapshot)
}

fn reject_all() -> Result(Nil, Error) {
  io.println("Looking for new snapshots...")
  use snapshots_folder <- result.try(find_snapshots_folder())
  use new_snapshots <- result.try(list_new_snapshots(in: snapshots_folder))

  case list.length(new_snapshots) {
    0 -> io.println("No new snapshots to reject.")
    1 -> io.println("Rejecting one new snapshot.")
    n -> io.println("Rejecting " <> int.to_string(n) <> " new snapshots.")
  }

  list.try_each(new_snapshots, reject_snapshot)
}

fn help() -> Nil {
  let version = ansi.green("🐦‍⬛ birdie ") <> "v" <> birdie_version
  io.println(version <> "\n\n" <> help_text())
}

fn help_text() -> String {
  ansi.yellow("USAGE:\n")
  <> "  gleam run -m birdie [ <SUBCOMMAND> ]\n\n"
  <> ansi.yellow("SUBCOMMANDS:\n")
  <> ansi.green("  review       ")
  <> "Review all new snapshots one by one\n"
  <> ansi.green("  accept-all   ")
  <> "Accept all new snapshots\n"
  <> ansi.green("  reject-all   ")
  <> "Reject all new snapshots\n"
  <> ansi.green("  help         ")
  <> "Show this help text\n"
}

fn unexpected_subcommand(subcommand: String) -> Nil {
  let error_message =
    ansi.bold("Error: ") <> "\"" <> subcommand <> "\" isn't a valid subcommand."

  io.println(ansi.red(error_message) <> "\n\n" <> help_text())
}

fn more_than_one_command(subcommands: List(String)) -> Nil {
  let error_message =
    ansi.bold("Error: ")
    <> "I can only run one subcommand at a time, but more than one were provided: "
    <> string.join(list.map(subcommands, fn(s) { "\"" <> s <> "\"" }), ", ")

  io.println(ansi.red(error_message) <> "\n\n" <> help_text())
}

fn report_status(result: Result(Nil, Error)) -> Nil {
  case result {
    Ok(Nil) -> io.println(ansi.green("🐦‍⬛ Done!"))
    Error(error) -> explain(error)
  }
}

// --- FFI ---------------------------------------------------------------------

/// Clear the screen.
///
@external(erlang, "birdie_ffi_erl", "clear")
fn clear() -> Nil

/// Move the cursor up a given number of lines.
///
@external(erlang, "birdie_ffi_erl", "cursor_up")
fn cursor_up(n: Int) -> Nil

/// Clear the line the cursor is currently on.
///
@external(erlang, "birdie_ffi_erl", "clear_line")
fn clear_line() -> Nil

fn terminal_width() -> Int {
  result.unwrap(do_terminal_width(), or: 80)
}

@external(erlang, "birdie_ffi_erl", "terminal_width")
@external(javascript, "./birdie_ffi_js.mjs", "terminal_width")
fn do_terminal_width() -> Result(Int, Nil) {
  // We have a default implementation that will fail on all other targets so
  // that it can be unwrapped to a default value and we stay compatible with
  // all future Gleam's targets.
  Error(Nil)
}
