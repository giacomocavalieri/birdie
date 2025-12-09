import birdie
import gleam/string
import gleeunit

pub fn main() {
  gleeunit.main()
}

pub fn hello_birdie_test() {
  "🐦‍⬛ smile for the birdie!"
  |> birdie.snap(title: "hello birdie test")
}

pub fn a_result_test() {
  string.inspect(Ok(11))
  |> birdie.snap(title: "a result test")
}

pub fn list_test() {
  "[ 1, 2, 3, 4 ]"
  |> birdie.snap(title: "list test")
}

pub fn complex_function_test() {
  "case wibble(wobble, woo) {
  True ->
    io.println(\"Phew, we don't have to launch the missiles...\")
  False -> {
    io.println(\"Not wibble!\")
    launch_missiles()
  }
}"
  |> birdie.snap(title: "complex function test")
}
