import birdie
import gleam/string
import gleeunit

pub fn main() {
  gleeunit.main()
}

pub fn hello_birdie_test() {
  "🐦‍⬛ smile for the birdie!"
  |> birdie.snap(title: "my first snapshot")
}

pub fn a_result_test() {
  string.inspect(Ok(11))
  |> birdie.snap(title: "my favourite number wrapped in a result")
}

pub fn list_test() {
  "[ 1, 2, 3, 4 ]"
  |> birdie.snap(title: "snapping a list of numbers")
}

pub fn complex_function_test() {
  "case foo(bar, baz) {
  True ->
    io.println(\"Phew, we don't have to launch the missiles...\")
  False -> {
    io.println(\"Not foo!\")
    launch_missiles()
  }
}"
  |> birdie.snap(title: "diffing a case expression")
}
