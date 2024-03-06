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

## FAQ

### What should my snapshots be named like?

A good idea is to give snapshots long descriptive titles that clearly state
what you're expecting to see when reviewing those.
Also all snapshots _must_ have unique names so that birdie won't mix those up,
so be careful when naming snapshots to not repeat the same title twice!

### How big should the snapshot's content be?

My recommendation is strive to have small and cohesive snapshots. Each
snapshot test should test one thing and one thing only. Having small snapshots
will make your life way easier during the review process!
It's better to review 10 small snpashots than a single huge one and you'll
see better, more focused diffs.

### Why is the snapshot content a `String`? I want to snapshot other things!

Birdie will only ever accept `String` values and it's up to you to turn your
own Gleam types into a `String` before snapping those: this way you have total
freedom and will be able to choose a format that makes sense to you and makes
things easier to review!

If you don't want to write serialisers for yourself and are ok with a default
look then I recommend you try using the [`pprint`](https://hexdocs.pm/pprint/)
package.
It's an awesome package that can turn any Gleam type into a pretty string that
will work perfectly with Birdie and produce nice diffs out of the box.
I'm sure most of the times you won't feel the need to use anything else!

```gleam
import birdie
import pprint

pub fn a_snapshot_test() {
  Ok([1, 2, 3])
  |> pprint.format
  // ^^^^^^^^^^^^^
  // pprint does all the hard work of turning
  // any value into a pretty string!
  |> birdie.snap(title: "a snapshot test using pprint's output")
}
```

## References

This package was heavily inspired by the excellent Rust library
[`insta`](https://insta.rs), do check it out!

## Contributing

If you think there's any way to improve this package, or if you spot a bug don't
be afraid to open PRs, issues or requests of any kind!
Any contribution is welcome 💜
