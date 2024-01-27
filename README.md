# 🐦‍⬛ Birdie - snapshot testing in Gleam

[![Package Version](https://img.shields.io/hexpm/v/birdie)](https://hex.pm/packages/birdie)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/birdie/)
![Supported targets](https://img.shields.io/badge/supports-all_targets-ffaff3)

Snapshot testing allows you to perform assertions without having to write the
expectation yourself. Birdie will store a snapshot of the expected value and
compare future runs of the same test against it. Imagine doing a
`should.equal(expected, got)` where you don't have to take care of writing the
expected output.

## Writing snapshot tests with Birdie

First you'll want to add the package to your dependencies:

```sh
gleam add --dev birdie
```

To write snapshot tests you can import the `birdie` module and use the
[`snap`](https://hexdocs.pm/birdie/birdie.html#snap) function:

```gleam
import gleeunit
import birdie

pub fn main() {
  gleeunit.main()
}

pub fn hello_birdie_test() {
  "🐦‍⬛ Smile for the birdie!"
  |> birdie.snap(title: "my first snapshot")
  // All snapshots must have a unique title!
} 
```

This will record a new snapshot with the given title and content. A snapshot
test will always fail on its first run until you review and accept it.
Once you've reviewed and accepted a snapshot, the test will fail only if the
snapshot's content changes; in that case you will be presented with a diff and
asked to review it once again.

A typical workflow will look like this:

- Run your tests
- If you have any new snapshots - or some of the snapshots have changed - some
  tests will fail
- Review all the new snapshots deciding if you want to keep the new version or
  the previously accepted one
- And don't forget to commit your snapshots! Those should be treated like
  code and checked with the vcs you're using

## Reviewing snapshots

Birdie also provides a CLI tool to help you in the review process: run
`gleam run -m birdie` in your project and birdie will help you interactively
review all your new snapshots.

> The CLI tool can also do more than just guide you through all your snapshots
> one by one. To check all the available options you can run
> `gleam run -m birdie help`

![image](https://github.com/giacomocavalieri/birdie/blob/main/birdie.gif?raw=true)

## References

This package was heavily inspired by the excellent Rust library
[`insta`](https://insta.rs), do check it out!

## Contributing

If you think there's any way to improve this package, or if you spot a bug don't
be afraid to open PRs, issues or requests of any kind!
Any contribution is welcome 💜
