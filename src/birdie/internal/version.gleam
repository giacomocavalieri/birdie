import gleam/int
import gleam/order
import gleam/result
import gleam/string

pub type Version {
  Version(major: Int, minor: Int, patch: Int)
}

pub fn new(major major: Int, minor minor: Int, patch patch: Int) -> Version {
  Version(major:, minor:, patch:)
}

pub fn parse(version: String) -> Result(Version, Nil) {
  case version |> string.trim |> string.split(on: ".") {
    [major, minor, patch] -> {
      use major <- result.try(int.parse(major))
      use minor <- result.try(int.parse(minor))
      use patch <- result.try(int.parse(patch))
      Ok(Version(major:, minor:, patch:))
    }
    _ -> Error(Nil)
  }
}

pub fn compare(one: Version, with other: Version) -> order.Order {
  use <- order.lazy_break_tie(int.compare(one.major, other.major))
  use <- order.lazy_break_tie(int.compare(one.minor, other.minor))
  int.compare(one.patch, other.patch)
}
