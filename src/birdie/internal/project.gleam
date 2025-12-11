import filepath
import gleam/result
import simplifile.{type FileError}
import tom

/// Returns the path to the project's root.
///
/// > ⚠️ This assumes that this is only ever run inside a Gleam's project and
/// > sooner or later it will reach a `gleam.toml` file.
/// > Otherwise this will end up in an infinite loop, I think.
///
pub fn find_root() -> Result(String, FileError) {
  do_find_root(".")
}

fn do_find_root(path: String) -> Result(String, FileError) {
  let manifest = filepath.join(path, "gleam.toml")
  case simplifile.is_file(manifest) {
    Ok(True) -> Ok(path)
    Ok(False) -> do_find_root(filepath.join(path, ".."))
    Error(reason) -> Error(reason)
  }
}

/// Returns the project's name as specified in its `gleam.toml`.
///
pub fn name() -> Result(String, FileError) {
  use root <- result.try(find_root())
  use file <- result.try(simplifile.read(filepath.join(root, "gleam.toml")))
  let assert Ok(toml) = tom.parse(file)
    as "running birdie in a gleam project with an invalid `gleam.toml` should be impossible"
  let assert Ok(name) = tom.get_string(toml, ["name"])
    as "`name` is a required field in `gleam.toml`, it should be impossible to run birdie on a project that doesn't have one"
  Ok(name)
}
