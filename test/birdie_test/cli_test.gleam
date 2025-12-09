import birdie/internal/cli.{
  type Command, Accept, Help, Reject, Review, UnknownCommand, UnknownOption,
  UnknownSubcommand,
}
import gleam/list
import gleam/option.{None, Some}
import gleam/string

pub fn unknown_top_level_command_test() {
  assert Error(UnknownCommand(command: "wibble")) == cli.parse(["wibble"])
  assert Error(UnknownCommand(command: "wibble"))
    == cli.parse(["wibble", "wobble"])
  assert Error(UnknownCommand(command: "wibble"))
    == cli.parse(["wibble", "--help"])
  assert Error(UnknownCommand(command: "wibble")) == cli.parse(["wibble", "-h"])
}

pub fn parse_review_test() {
  // No arguments are interpreted as the review command
  assert Ok(Review) == cli.parse([])
  assert Ok(Help(Some(Review))) == cli.parse(["--help"])
  assert Ok(Help(Some(Review))) == cli.parse(["-h"])

  // Explicitly using the review command
  assert Ok(Review) == cli.parse(["review"])
  assert Ok(Help(Some(Review))) == cli.parse(["review", "--help"])
  assert Ok(Help(Some(Review))) == cli.parse(["review", "-h"])

  // Unknown subcommands and options
  assert Error(UnknownSubcommand(Review, "wibble"))
    == cli.parse(["review", "wibble"])
  assert Error(UnknownOption(Review, "-w")) == cli.parse(["-w"])
  assert Error(UnknownOption(Review, "-w")) == cli.parse(["review", "-w"])
  assert Error(UnknownOption(Review, "--wibble")) == cli.parse(["--wibble"])
  assert Error(UnknownOption(Review, "--wibble"))
    == cli.parse(["review", "--wibble"])
}

pub fn parse_accept_test() {
  // Explicitly using the review command
  assert Ok(Accept) == cli.parse(["accept"])
  assert Ok(Help(Some(Accept))) == cli.parse(["accept", "--help"])
  assert Ok(Help(Some(Accept))) == cli.parse(["accept", "-h"])

  // Unknown subcommands and options
  assert Error(UnknownSubcommand(Accept, "wibble"))
    == cli.parse(["accept", "wibble"])
  assert Error(UnknownOption(Accept, "-w")) == cli.parse(["accept", "-w"])
  assert Error(UnknownOption(Accept, "--wibble"))
    == cli.parse(["accept", "--wibble"])
}

pub fn parse_reject_test() {
  // Explicitly using the review command
  assert Ok(Reject) == cli.parse(["reject"])
  assert Ok(Help(Some(Reject))) == cli.parse(["reject", "--help"])
  assert Ok(Help(Some(Reject))) == cli.parse(["reject", "-h"])

  // Unknown subcommands and options
  assert Error(UnknownSubcommand(Reject, "wibble"))
    == cli.parse(["reject", "wibble"])
  assert Error(UnknownOption(Reject, "-w")) == cli.parse(["reject", "-w"])
  assert Error(UnknownOption(Reject, "--wibble"))
    == cli.parse(["reject", "--wibble"])
}

pub fn parse_help_test() {
  // Explicitly using the review command
  assert Ok(Help(None)) == cli.parse(["help"])
  assert Ok(Help(None)) == cli.parse(["help", "--help"])
  assert Ok(Help(None)) == cli.parse(["help", "-h"])
}

pub fn all_commands_test() {
  assert cli.all_commands() == all_known_commands([])
}

fn all_known_commands(all: List(Command)) -> List(String) {
  case all {
    [] -> all_known_commands([Accept, ..all])
    [Accept, ..] -> all_known_commands([Help(None), ..all])
    [Help(..), ..] -> all_known_commands([Reject, ..all])
    [Reject, ..] -> all_known_commands([Review, ..all])
    [Review, ..] ->
      list.map(all, command_to_string)
      |> list.sort(string.compare)
  }
}

fn command_to_string(command: Command) {
  case command {
    Accept -> "accept"
    Help(..) -> "help"
    Reject -> "reject"
    Review -> "review"
  }
}
