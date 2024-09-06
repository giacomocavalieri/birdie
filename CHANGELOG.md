# Changelog

## Unreleased

- 🐛 Fixed a bug where birdie would strip `\r\n` out of a snapshot content

## 1.2.2 - 2024-09-06

- 🐛 Fixed a bug where snapshot tests would fail on Windows

## v1.2.1 - 2024-08-20

- ⬆️ Update `stdlib` to `>= 0.40.0 and < 1.0.0`

## v1.2.0 - 2024-08-12

- ✨ Birdie can now suggest and run the correct command if it can tell you've
  made a typo

## v1.1.8 - 2024-05-28

- ⬆️ Update `stdlib` to `>= 0.39.0 and < 1.0.0`

## v1.1.7 - 2024-05-28

- ⬆️ Update `simplifile` to `>= 2.0.1 and < 3.0.0`

## v1.1.6 - 2024-05-28

- ⬆️ Change dependencies contraints

## v1.1.5 - 2024-05-10

- 🧑🏻‍💻 No longer fail the process review if Glance cannot parse a test module.
  Instead title data will simply be omitted.

## v1.1.4 - 2024-04-26

- 🐛 Ignore non Gleam files in the test directory

## v1.1.3 - 2024-04-20

- ⬆️ Update glance dependency

## v1.1.2 - 2024-04-11

- 🐛 Make sure no snapshot with an empty title is accepted

## v1.1.1 - 2024-04-07

- ⬆️ Update filepath from `~> 0.1` to `~> 1.0`
- ➖ Drop `gap` dependency
- ➖ Drop `gleeunit` dependency

## v1.1.0 - 2024-03-12

- ✨ Fail the review if there's tests with duplicate titles

## v1.0.4 - 2024-02-01

- ➖ Drop the `glam` dependency

## v1.0.3 - 2024-02-01

- 🧑🏻‍💻 Improve diffs look

## v1.0.2 - 2024-01-28

- 📝 Improve `birdie.main` documentation

## v1.0.1 - 2024-01-27

- 🐛 Fix a bug with js FFI code

## v1.0.0 - 2024-01-27

- 🎉 First release!
