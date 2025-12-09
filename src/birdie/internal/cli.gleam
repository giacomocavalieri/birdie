import edit_distance
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam_community/ansi

pub type Command {
  Review
  Accept
  Reject
  Help(command: Option(Command))
}

pub type Error {
  UnknownCommand(command: String)
  UnknownSubcommand(command: Command, subcommand: String)
  UnknownOption(command: Command, option: String)
}

pub fn parse(args: List(String)) -> Result(Command, Error) {
  let #(commands, options) =
    list.partition(args, fn(arg) {
      case arg {
        "--" <> _ | "-" <> _ -> False
        _ -> True
      }
    })

  case commands {
    [] -> Review |> or_help(options)

    ["review"] -> Review |> or_help(options)
    ["review", subcommand, ..] ->
      Error(UnknownSubcommand(command: Review, subcommand:))

    ["reject"] -> Reject |> or_help(options)
    ["reject", subcommand, ..] ->
      Error(UnknownSubcommand(command: Reject, subcommand:))

    ["accept"] -> Accept |> or_help(options)
    ["accept", subcommand, ..] ->
      Error(UnknownSubcommand(command: Accept, subcommand:))

    ["help", ..] -> Ok(Help(None))

    [command, ..] -> Error(UnknownCommand(command:))
  }
}

fn or_help(command: Command, options: List(String)) -> Result(Command, Error) {
  case list.find(options, one_that: fn(option) { !is_help(option) }) {
    Ok(option) -> Error(UnknownOption(command:, option:))
    Error(_) ->
      case list.any(options, is_help) {
        True -> Ok(Help(Some(command)))
        False -> Ok(command)
      }
  }
}

fn is_help(option: String) -> Bool {
  case option {
    "-h" | "--help" -> True
    _ -> False
  }
}

/// This will return one of the allowed commands if there's one that's similar
/// enough to the given one.
///
pub fn similar_command(to command: String) -> Result(String, Nil) {
  list.filter_map(all_commands(), fn(valid_command) {
    case edit_distance.levenshtein(command, valid_command) {
      distance if distance <= 3 -> Ok(#(valid_command, distance))
      _ -> Error(Nil)
    }
  })
  |> list.sort(fn(one, other) { int.compare(one.1, other.1) })
  |> list.first
  |> result.map(fn(pair) { pair.0 })
}

pub fn all_commands() -> List(String) {
  ["accept", "help", "reject", "review"]
}

// ERROR MESSAGES --------------------------------------------------------------

pub fn unknown_command_error(
  birdie_version: String,
  command: String,
  show_help_text: Bool,
) -> String {
  let message =
    ansi.red("Error: ")
    <> style_invalid_value(command)
    <> " is not a valid command"

  case show_help_text {
    False -> message
    True -> message <> "\n\n" <> help_text(birdie_version, None)
  }
}

pub fn unknown_subcommand_error(
  birdie_version: String,
  command: Command,
  subcommand: String,
) -> String {
  ansi.red("Error: ")
  <> style_invalid_value(subcommand)
  <> " is not a valid subcommand\n\n"
  <> command_help_text(birdie_version, command)
}

pub fn unknown_option_error(
  birdie_version: String,
  command: Command,
  option: String,
) -> String {
  ansi.red("Error: ")
  <> style_invalid_value(option)
  <> " is not a valid option\n\n"
  <> help_text(birdie_version, for: Some(command))
}

fn style_invalid_value(value: String) -> String {
  ansi.yellow("'" <> value <> "'")
}

// HELP TEXTS ------------------------------------------------------------------

pub fn help_text(birdie_version: String, for command: Option(Command)) -> String {
  case command {
    None -> help_help_text(birdie_version)
    Some(command) -> subcommand_help_text(birdie_version, command)
  }
}

/// Returns the help text describing a given command with its subcommands.
/// So this goes as in depth as possible.
///
fn subcommand_help_text(birdie_version: String, command: Command) -> String {
  case command {
    Help(command:) -> help_text(birdie_version, for: command)
    Review -> command_help_text(birdie_version, command)
    Accept -> command_help_text(birdie_version, command)
    Reject -> command_help_text(birdie_version, command)
  }
}

/// Returns the help text for the given command _ignoring all of its subcommands
/// that might be there!_
///
fn command_help_text(birdie_version: String, command: Command) -> String {
  case command {
    Help(..) -> help_help_text(birdie_version)
    Accept -> accept_help_text()
    Reject -> reject_help_text()
    Review -> review_help_text()
  }
}

fn review_help_text() -> String {
  usage(["review"], None)
  <> "\n\n"
  <> "Review all new snapshots one by one"
  <> "\n\n"
  <> options()
}

fn reject_help_text() -> String {
  usage(["reject"], None)
  <> "\n\n"
  <> "Reject all new snapshots"
  <> "\n\n"
  <> options()
}

fn accept_help_text() -> String {
  usage(["accept"], None)
  <> "\n\n"
  <> "Accept all new snapshots"
  <> "\n\n"
  <> options()
}

fn help_help_text(birdie_version: String) -> String {
  { ansi.green("🐦‍⬛ birdie ") <> "v" <> birdie_version }
  <> "\n\n"
  <> usage([], Some(Command))
  <> "\n\n"
  <> command_menu()
  <> "\n\n"
  <> options()
}

fn command_menu() -> String {
  ansi.yellow("Commands:\n")
  <> ansi.green("  review   ")
  <> "review all new snapshots one by one\n"
  <> ansi.green("  accept   ")
  <> "accept all new snapshots\n"
  <> ansi.green("  reject   ")
  <> "reject all new snapshots\n"
  <> ansi.green("  help     ")
  <> "print this help text"
}

type ArgumentsKind {
  Command
  Subcommand
}

fn usage(
  commands: List(String),
  arguments_kind: Option(ArgumentsKind),
) -> String {
  let command_placeholder = case arguments_kind {
    None -> " "
    Some(Command) -> ansi.dim(" <COMMAND> ")
    Some(Subcommand) -> ansi.dim(" <SUBCOMMAND> ")
  }
  let commands = case commands {
    [] -> ""
    [_, ..] -> " " <> ansi.green(string.join(commands, with: " "))
  }

  ansi.yellow("Usage: ")
  <> "gleam run -m"
  <> ansi.green(" birdie")
  <> commands
  <> command_placeholder
  <> ansi.dim("[OPTIONS]")
}

fn options() -> String {
  let help_option = ansi.green("-h") <> ", " <> ansi.green("--help")
  ansi.yellow("Options:") <> "\n  " <> help_option <> "   print this help text"
}
