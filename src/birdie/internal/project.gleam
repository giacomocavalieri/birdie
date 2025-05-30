import filepath
import simplifile.{type FileError}

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
