import birdie/internal/project
import filepath
import glance.{type Span}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import simplifile

/// A data structure to hold info about all the titles gathered from a project's
/// test modules.
///
pub opaque type Titles {
  Titles(
    /// All the birdie literal titles defined in the module. For example:
    /// `"This is a literal title"`
    ///
    literals: Dict(String, TestInfo),
  )
}

pub type TestInfo {
  TestInfo(file: String, test_name: String)
}

/// A match you can get when looking for a title.
///
pub type Match {
  LiteralMatch(info: TestInfo)
}

/// All the possible errors that can occur when gathering the titles from a
/// project's test modules.
///
pub type Error {
  CannotFindProjectRoot(reason: simplifile.FileError)
  CannotReadTestDirectory(reason: simplifile.FileError)
  CannotReadTestFile(reason: simplifile.FileError, file: String)
  DuplicateLiteralTitles(title: String, one: TestInfo, other: TestInfo)
}

type BirdieImport {
  Unqualified(module_alias: String)
  Qualified(module_alias: String, snap_alias: String)
  Discarded(snap_alias: String)
}

type Title {
  Literal(title: String)
  // TODO: We could be smarter and detect titles that are not literals, but it
  // seems like a lot of work
}

// --- TITLES CREATION ---------------------------------------------------------

pub fn new() -> Titles {
  Titles(literals: dict.new())
}

fn add_literal_title(titles: Titles, title: String, info: TestInfo) -> Titles {
  let literals = dict.insert(titles.literals, title, info)
  Titles(literals:)
}

pub fn literals(titles: Titles) -> Dict(String, TestInfo) {
  let Titles(literals:) = titles
  literals
}

// --- TITLE LOOKUP ------------------------------------------------------------

pub fn find(titles: Titles, title title: String) -> Result(Match, Nil) {
  result.map(dict.get(titles.literals, title), LiteralMatch)
}

// --- TITLE GATHERING ---------------------------------------------------------

pub fn from_test_directory() -> Result(Titles, Error) {
  use root <- try(project.find_root(), CannotFindProjectRoot)
  let test_directory = filepath.join(root, "test")
  let get_files = simplifile.get_files(test_directory)
  use files <- try(get_files, CannotReadTestDirectory)

  use titles, file <- list.try_fold(over: files, from: new())
  let is_gleam_file = filepath.extension(file) == Ok("gleam")
  use <- bool.guard(when: !is_gleam_file, return: Ok(titles))

  use raw_module <- try(simplifile.read(file), CannotReadTestFile(_, file))
  case glance.module(raw_module) {
    Ok(module) -> from_module(titles, file, module)
    Error(_) -> Ok(titles)
  }
}

pub fn from_module(
  titles: Titles,
  name: String,
  module: glance.Module,
) -> Result(Titles, Error) {
  use birdie_import <- try_or(birdie_import(module), return: Ok(titles))
  use titles, function <- list.try_fold(over: module.functions, from: titles)
  let body = function.definition.body
  use titles, expression <- try_fold_statements(body, titles)

  // We see if the expression is a call to the `birdie.snap` function.
  case snap_call(birdie_import, expression) {
    // We have found a call to `birdie.snap` where the title is a literal
    // string.
    Ok(Literal(title:)) -> {
      let info = TestInfo(file: name, test_name: function.definition.name)
      case find(titles, title) {
        Error(Nil) -> Ok(add_literal_title(titles, title, info))
        Ok(LiteralMatch(other_info)) ->
          Error(DuplicateLiteralTitles(title, info, other_info))
      }
    }

    // The call is in a format we do not currently support.
    Error(Nil) -> Ok(titles)
  }
}

fn birdie_import(module: glance.Module) -> Result(BirdieImport, Nil) {
  use nil, import_ <- list.fold_until(over: module.imports, from: Error(Nil))
  case import_.definition {
    glance.Import(
      location: _,
      module: "birdie",
      alias: birdie_alias,
      unqualified_types: _,
      unqualified_values:,
    ) -> {
      case birdie_alias {
        Some(glance.Discarded(_)) ->
          case imported_snap(unqualified_values) {
            Ok(snap_alias) -> list.Stop(Ok(Discarded(snap_alias)))
            Error(_) -> list.Stop(Error(Nil))
          }

        Some(glance.Named(module_name)) ->
          case imported_snap(unqualified_values) {
            Ok(snap_alias) -> list.Stop(Ok(Qualified(module_name, snap_alias)))
            Error(_) -> list.Stop(Ok(Unqualified(module_name)))
          }

        None ->
          case imported_snap(unqualified_values) {
            Ok(snap_alias) -> list.Stop(Ok(Qualified("birdie", snap_alias)))
            Error(_) -> list.Stop(Ok(Unqualified("birdie")))
          }
      }
    }
    _ -> list.Continue(nil)
  }
}

fn imported_snap(
  values: List(glance.UnqualifiedImport),
) -> Result(String, Nil) {
  use nil, value <- list.fold_until(over: values, from: Error(Nil))
  case value {
    glance.UnqualifiedImport(name: "snap", alias: None) -> list.Stop(Ok("snap"))
    glance.UnqualifiedImport(name: "snap", alias: Some(alias)) ->
      list.Stop(Ok(alias))
    _ -> list.Continue(nil)
  }
}

fn snap_call(
  birdie_import: BirdieImport,
  expression: glance.Expression,
) -> Result(Title, Nil) {
  case expression {
    glance.Call(
      location:,
      function:,
      arguments: [
        glance.UnlabelledField(title),
        glance.LabelledField("content", _location, _snapshot_content),
      ],
    )
    | // A direct function call to the `birdie.snap` function where the first
      // argument is the unlabelled content. This means that the second argument
      // must be the title - labelled or not.
      glance.Call(
        location:,
        function:,
        arguments: [
          glance.UnlabelledField(_snapshot_content),
          glance.UnlabelledField(title),
        ],
      )
    | glance.Call(
        location:,
        function:,
        arguments: [
          glance.UnlabelledField(_snapshot_content),
          glance.LabelledField("title", _location, title),
        ],
      )
    | // A direct function call to the `birdie.snap` function where the first
      // argument is the labelled content. This means that the second argument
      // must be the title - labelled or not.
      glance.Call(
        location:,
        function:,
        arguments: [
          glance.LabelledField("content", _location, _snapshot_content),
          glance.UnlabelledField(title),
        ],
      )
    | glance.Call(
        location:,
        function:,
        arguments: [
          glance.LabelledField("content", _location, _snapshot_content),
          glance.LabelledField("title", _location, title),
        ],
      )
    | // A direct function call to the `birdie.snap` function where the first
      // argument is the labelled title.
      glance.Call(
        location:,
        function:,
        arguments: [
          glance.LabelledField("title", _location, title),
          _content_field,
        ],
      )
    | // A call to the `birdie.snap` function where the title is piped into it
      // and the content is passed as a labelled argument.
      glance.BinaryOperator(
        location:,
        name: glance.Pipe,
        left: title,
        right: glance.Call(
          location: _,
          function:,
          arguments: [
            glance.LabelledField("content", _location, _snapshot_content),
          ],
        ),
      )
    | // A call to the `birdie.snap` function where the content is piped into
      // it and the title is passed as an argument - labelled or not.
      glance.BinaryOperator(
        location:,
        name: glance.Pipe,
        left: _snapshot_content,
        right: glance.Call(
          location: _,
          function:,
          arguments: [glance.UnlabelledField(title)],
        ),
      )
    | glance.BinaryOperator(
        location:,
        name: glance.Pipe,
        left: _snapshot_content,
        right: glance.Call(
          location: _,
          function:,
          arguments: [glance.LabelledField("title", _location, title)],
        ),
      )
    | // We pipe into `title: _`, since we're using a label we don't have to
      // check the position.
      glance.BinaryOperator(
        location:,
        name: glance.Pipe,
        left: title,
        right: glance.FnCapture(
          location: _,
          function:,
          label: Some("title"),
          arguments_before: _,
          arguments_after: _,
        ),
      )
    | // A call to the `birdie.snap` function where the title is piped into it
      // and the content is passed as an argument. This must be done using a
      // function capture.
      glance.BinaryOperator(
        location:,
        name: glance.Pipe,
        left: title,
        right: glance.FnCapture(
          location: _,
          function:,
          label: _,
          arguments_before: [glance.UnlabelledField(_snapshot_content)],
          arguments_after: [],
        ),
      ) -> {
      let is_snap_function = is_snap_function(function, birdie_import)
      use <- bool.guard(when: !is_snap_function, return: Error(Nil))
      expression_to_snap_title(location, title)
    }
    // Everything else is in a format birdie currently doesn't support.
    _ -> Error(Nil)
  }
}

fn is_snap_function(
  expression: glance.Expression,
  birdie_import: BirdieImport,
) -> Bool {
  let is_a_call_to_snap = fn(module, name) {
    case module, birdie_import {
      None, Unqualified(module_alias: _) -> False
      None, Qualified(module_alias: _, snap_alias: snap) -> snap == name
      None, Discarded(snap_alias: snap) -> snap == name
      Some(module), Qualified(module_alias: birdie, snap_alias: snap) ->
        module <> "." <> name == birdie <> "." <> snap
      Some(module), Unqualified(module_alias: birdie) ->
        module <> "." <> name == birdie <> ".snap"
      Some(_), Discarded(snap_alias: _) -> False
    }
  }

  case expression {
    glance.Variable(_location, name) -> is_a_call_to_snap(None, name)
    glance.FieldAccess(_location, glance.Variable(_location, module), name) ->
      is_a_call_to_snap(Some(module), name)
    _ -> False
  }
}

fn expression_to_snap_title(
  _call_location: Span,
  expression: glance.Expression,
) -> Result(Title, Nil) {
  case expression {
    glance.String(_, title) -> Ok(Literal(title:))
    _ -> Error(Nil)
  }
}

// --- AST FOLDING -------------------------------------------------------------

fn try_fold_statements(
  statements: List(glance.Statement),
  acc: a,
  fun: fn(a, glance.Expression) -> Result(a, b),
) -> Result(a, b) {
  use acc, statement <- list.try_fold(over: statements, from: acc)
  case statement {
    glance.Use(location: _, patterns: _, function: expression)
    | glance.Assert(location: _, expression:, message: None)
    | glance.Assignment(
        location: _,
        kind: _,
        pattern: _,
        annotation: _,
        value: expression,
      )
    | glance.Expression(expression) -> try_fold_expression(expression, acc, fun)

    glance.Assert(location: _, expression:, message: Some(message)) -> {
      case try_fold_expression(expression, acc, fun) {
        Ok(acc) -> try_fold_expression(message, acc, fun)
        Error(_) as e -> e
      }
    }
  }
}

fn try_fold_expression(
  expression: glance.Expression,
  acc: a,
  fun: fn(a, glance.Expression) -> Result(a, b),
) -> Result(a, b) {
  use acc <- result.try(fun(acc, expression))
  case expression {
    glance.Int(_, _)
    | glance.Float(_, _)
    | glance.String(_, _)
    | glance.Variable(_, _)
    | glance.Panic(_, _)
    | glance.Todo(_, _)
    | glance.Echo(_, None, _) -> Ok(acc)

    glance.NegateInt(_location, expression)
    | glance.NegateBool(_location, expression)
    | glance.Echo(_location, Some(expression), _message)
    | glance.FieldAccess(location: _, container: expression, label: _)
    | glance.TupleIndex(location: _, tuple: expression, index: _) ->
      try_fold_expression(expression, acc, fun)

    glance.Block(_location, statements) ->
      try_fold_statements(statements, acc, fun)

    glance.Tuple(_location, expressions)
    | glance.List(_location, expressions, None) ->
      try_fold_expressions(expressions, acc, fun)

    glance.List(location: _, elements:, rest: Some(rest)) -> {
      use acc <- result.try(try_fold_expressions(elements, acc, fun))
      try_fold_expression(rest, acc, fun)
    }

    glance.Fn(location: _, arguments: _, return_annotation: _, body: statements) ->
      try_fold_statements(statements, acc, fun)

    glance.RecordUpdate(
      location: _,
      module: _,
      constructor: _,
      record:,
      fields:,
    ) -> {
      use acc <- result.try(try_fold_expression(record, acc, fun))
      use acc, field <- list.try_fold(over: fields, from: acc)
      let glance.RecordUpdateField(label: _, item:) = field
      case item {
        Some(item) -> try_fold_expression(item, acc, fun)
        None -> Ok(acc)
      }
    }

    glance.Call(location: _, function:, arguments:) -> {
      use acc <- result.try(try_fold_expression(function, acc, fun))
      try_fold_fields(arguments, acc, fun)
    }

    glance.FnCapture(
      location: _,
      label: _,
      function:,
      arguments_before:,
      arguments_after:,
    ) -> {
      use acc <- result.try(try_fold_expression(function, acc, fun))
      use acc <- result.try(try_fold_fields(arguments_before, acc, fun))
      try_fold_fields(arguments_after, acc, fun)
    }

    glance.BitString(location: _, segments:) -> {
      use acc, #(segment, options) <- list.try_fold(over: segments, from: acc)
      use acc <- result.try(try_fold_expression(segment, acc, fun))
      use acc, option <- list.try_fold(over: options, from: acc)
      case option {
        glance.SizeValueOption(expression) ->
          try_fold_expression(expression, acc, fun)
        _ -> Ok(acc)
      }
    }

    glance.Case(location: _, subjects:, clauses:) -> {
      use acc <- result.try(try_fold_expressions(subjects, acc, fun))
      try_fold_clauses(clauses, acc, fun)
    }

    glance.BinaryOperator(location: _, name: _, left:, right:) -> {
      use acc <- result.try(try_fold_expression(left, acc, fun))
      try_fold_expression(right, acc, fun)
    }
  }
}

fn try_fold_fields(
  fields: List(glance.Field(glance.Expression)),
  acc: a,
  fun: fn(a, glance.Expression) -> Result(a, b),
) -> Result(a, b) {
  use acc, field <- list.try_fold(over: fields, from: acc)
  case field {
    glance.LabelledField(label: _, label_location: _, item:)
    | glance.UnlabelledField(item:) -> try_fold_expression(item, acc, fun)
    glance.ShorthandField(label: _, location: _) -> Ok(acc)
  }
}

fn try_fold_clauses(
  clauses: List(glance.Clause),
  acc: a,
  fun: fn(a, glance.Expression) -> Result(a, b),
) -> Result(a, b) {
  use acc, clause <- list.try_fold(over: clauses, from: acc)
  case clause {
    glance.Clause(patterns: _, guard: None, body:) ->
      try_fold_expression(body, acc, fun)
    glance.Clause(patterns: _, guard: Some(guard), body:) -> {
      use acc <- result.try(try_fold_expression(guard, acc, fun))
      try_fold_expression(body, acc, fun)
    }
  }
}

fn try_fold_expressions(
  expressions: List(glance.Expression),
  acc: a,
  fun: fn(a, glance.Expression) -> Result(a, b),
) -> Result(a, b) {
  use acc, expression <- list.try_fold(expressions, acc)
  try_fold_expression(expression, acc, fun)
}

// --- UTILITIES ---------------------------------------------------------------

fn try_or(result: Result(a, b), return default: c, with fun: fn(a) -> c) -> c {
  case result {
    Ok(a) -> fun(a)
    Error(_) -> default
  }
}

fn try(
  result: Result(a, b),
  map_error: fn(b) -> c,
  fun: fn(a) -> Result(d, c),
) -> Result(d, c) {
  case result {
    Ok(a) -> fun(a)
    Error(e) -> Error(map_error(e))
  }
}
