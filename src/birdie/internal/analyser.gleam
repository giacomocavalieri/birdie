import birdie/internal/position.{type Map, type Position}
import glance.{type Span}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/uri.{type Uri}

pub opaque type Analyser {
  Analyser(
    /// A dict from module name to the titles inside that module.
    modules: Dict(Uri, AnalysedModule),
    /// A dictionary from snapshot literal title to a dictionary mapping from
    /// modules to locations in that module where the title is used.
    /// This keeps track of where each title is used!
    literal_titles: Dict(String, Dict(Uri, Set(Span))),
  )
}

pub type Error {
  TitleAlreadyInUse(module: AnalysedModule, title_span: Span)
}

pub type Warning {
  NonLiteralTitle(module: AnalysedModule, title_span: Span)
}

pub type Module {
  Module(path: Uri, source: String)
}

pub type AnalysedModule {
  AnalysedModule(path: Uri, snapshots: List(SnapshotTest), line_numbers: Map)
}

pub type SnapshotTest {
  SnapshotTest(
    /// The title used for the snapshot. For example:
    ///
    /// ```gleam
    /// birdie.snap(todo, title: "wibble")
    /// //                       ^^^^^^^^ This is the title!
    /// ```
    ///
    title: SnapshotTitle,
    /// The span covering just the title of the `birdie.snap` call. For example:
    ///
    /// ```gleam
    ///    birdie.snap(todo, title: "wibble")
    /// //                          ^^^^^^^^ This!
    /// ```
    ///
    title_span: Span,
    /// The span covering the whole `birdie.snap` call. For example:
    ///
    /// ```gleam
    ///    birdie.snap(todo, todo)
    /// // ^^^^^^^^^^^^^^^^^^^^^^^ This!
    /// ```
    ///
    /// With pipelines, it covers the entire pipeline!
    ///
    /// ```gleam
    ///    todo |> birdie.snap(title: "wibble")
    /// // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ This!
    /// ```
    ///
    call_span: Span,
  )
}

pub type SnapshotTitle {
  LiteralTitle(title: String)
  ExpressionTitle
}

pub fn new() -> Analyser {
  Analyser(modules: dict.new(), literal_titles: dict.new())
}

pub fn remove_module(analyser: Analyser, module: Uri) -> Analyser {
  let Analyser(modules:, literal_titles:) = analyser

  case dict.get(modules, module) {
    // We were asked to remove a module which wasn't analysed in the first
    // place, or that has already been removed. We do nothing!
    Error(_) -> analyser
    // Found the module we should remove.
    Ok(module) -> {
      // We need to remove it from the analysed modules...
      let modules = dict.delete(modules, module.path)
      // ...and we also need to remove its names from all the name references!
      // In order to do that we go over all the names the module defined and
      // update them removing the reference.
      let literal_titles =
        list.fold(module.snapshots, literal_titles, fn(names, snapshot) {
          remove_snapshot_title(snapshot, in: module, from: names)
        })

      Analyser(modules:, literal_titles:)
    }
  }
}

fn remove_snapshot_title(
  snapshot: SnapshotTest,
  in module: AnalysedModule,
  from names: Dict(String, Dict(Uri, Set(Span))),
) -> Dict(String, Dict(Uri, Set(Span))) {
  case snapshot.title {
    // If the snapshot doesn't have a literal title then it can't be part of the
    // names, we just skip it!
    ExpressionTitle(..) -> names
    // Otherwise we need to remove it from the names.
    LiteralTitle(title:) ->
      case dict.get(names, title) {
        Error(_) -> names
        Ok(module_to_spans) -> {
          let module_to_spans = dict.delete(module_to_spans, module.path)
          dict.insert(names, title, module_to_spans)
        }
      }
  }
}

pub fn errors(analyser: Analyser) -> List(Error) {
  dict.fold(analyser.literal_titles, [], fn(acc, _title, references) {
    let errors =
      dict.fold(references, [], fn(acc, module, references) {
        set.fold(references, acc, fn(acc, reference) {
          case dict.get(analyser.modules, module) {
            Ok(module) -> [TitleAlreadyInUse(module, reference), ..acc]
            Error(_) -> acc
          }
        })
      })

    case errors {
      [] | [_] -> acc
      [_, _, ..] -> errors |> list.append(acc)
    }
  })
}

pub fn warnings(analyser: Analyser) -> List(Warning) {
  dict.fold(analyser.modules, [], fn(acc, _, module) {
    list.fold(module.snapshots, acc, fn(acc, snapshot) {
      case snapshot.title {
        LiteralTitle(_) -> acc
        ExpressionTitle -> [
          NonLiteralTitle(module:, title_span: snapshot.title_span),
          ..acc
        ]
      }
    })
  })
}

pub fn find_test(
  analyser: Analyser,
  in module: Uri,
  hovered position: Position,
) -> Result(#(Map, SnapshotTest), Nil) {
  // We first get the module inside of which the hover is taking place.
  use module <- result.try(dict.get(analyser.modules, module))
  // If it has been analysed, we look for the snapshot that is being hovered.
  list.find_map(module.snapshots, fn(snapshot) {
    let index = position.to_byte_index(module.line_numbers, position)
    let snapshot_span = snapshot.call_span
    let is_hovered = snapshot_span.start <= index && index < snapshot_span.end
    case is_hovered {
      True -> Ok(#(module.line_numbers, snapshot))
      False -> Error(Nil)
    }
  })
}

// ---- MODULE ANALYSIS --------------------------------------------------------

pub fn analyse(analyser: Analyser, module module: Module) -> Analyser {
  case analyse_module(module) {
    Error(_) -> analyser
    Ok(module) -> add_analysed_module(analyser, module)
  }
}

fn add_analysed_module(analyser: Analyser, module: AnalysedModule) -> Analyser {
  let Analyser(modules:, literal_titles:) = analyser

  // We add the module to the analysed ones...
  let modules = dict.insert(modules, module.path, module)

  // ...and we keep track of all the literal snapshot names it defines
  let literal_titles =
    list.group(module.snapshots, fn(snapshot) { snapshot.title })
    |> dict.fold(literal_titles, fn(literal_titles, title, snapshots) {
      // We only care about snapshots that share a literal title!
      case title {
        ExpressionTitle -> literal_titles
        LiteralTitle(title:) -> {
          let spans =
            list.map(snapshots, fn(snapshot) { snapshot.title_span })
            |> set.from_list

          dict.upsert(literal_titles, title, fn(references) {
            case references {
              None -> dict.from_list([#(module.path, spans)])
              Some(references) -> dict.insert(references, module.path, spans)
            }
          })
        }
      }
    })

  Analyser(modules:, literal_titles:)
}

/// Analyses a module, returning `Error(Nil)` if the module is not using
/// `birdie.snap` at all, or if it can't be parsed for any reason.
fn analyse_module(module: Module) -> Result(AnalysedModule, Nil) {
  // We first need to parse the module, if it contains any error there's not
  // much we can do!
  use parsed_module <- result.try(
    glance.module(module.source)
    |> result.replace_error(Nil),
  )
  // We then figure out how the `birdie.snap` function might be called inside
  // the module. If `birdie` isn't imported at all we're done! There's nothing
  // to do for the module.
  use snap_usage <- result.try(snap_usage(for: parsed_module))
  // We now go over all expressions in the module, collecting all snapshot tests
  // we can find
  let snapshots = {
    use snapshots, function <- list.fold(parsed_module.functions, [])
    let body = function.definition.body

    // Each function has a new empty scope.
    let scope = Scope(set.new())
    use snapshots, scope, expression <- fold_statements(body, scope, snapshots)
    case snapshot_test(snap_usage, scope, expression) {
      Ok(snapshot) -> [snapshot, ..snapshots]
      Error(_) -> snapshots
    }
  }

  case snapshots {
    [] -> Error(Nil)
    [_, ..] ->
      Ok(AnalysedModule(
        path: module.path,
        snapshots:,
        line_numbers: position.map_from_source(module.source),
      ))
  }
}

fn snapshot_test(
  snap_usage: SnapUsage,
  scope: Scope,
  expression: glance.Expression,
) -> Result(SnapshotTest, Nil) {
  case expression {
    // `func(title, content: content)`
    glance.Call(
      location: call_span,
      function:,
      arguments: [
        glance.UnlabelledField(title),
        glance.LabelledField("content", _location, _snapshot_content),
      ],
    )
    | // `func(content, title)`
      glance.Call(
        location: call_span,
        function:,
        arguments: [
          glance.UnlabelledField(_snapshot_content),
          glance.UnlabelledField(title),
        ],
      )
    | // `func(content, title: title)`
      glance.Call(
        location: call_span,
        function:,
        arguments: [
          glance.UnlabelledField(_snapshot_content),
          glance.LabelledField("title", _location, title),
        ],
      )
    | // `func(content: content, title)`
      glance.Call(
        location: call_span,
        function:,
        arguments: [
          glance.LabelledField("content", _location, _snapshot_content),
          glance.UnlabelledField(title),
        ],
      )
    | // `func(content: content, title: title)`
      glance.Call(
        location: call_span,
        function:,
        arguments: [
          glance.LabelledField("content", _location, _snapshot_content),
          glance.LabelledField("title", _location, title),
        ],
      )
    | // `func(title: title, content)`, `func(title: title, content: content)`
      glance.Call(
        location: call_span,
        function:,
        arguments: [
          glance.LabelledField("title", _location, title),
          _content_field,
        ],
      )
    | // `title |> func(content: content)`
      glance.BinaryOperator(
        location: call_span,
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
    | // `content |> func(title)`
      glance.BinaryOperator(
        location: call_span,
        name: glance.Pipe,
        left: _snapshot_content,
        right: glance.Call(
          location: _,
          function:,
          arguments: [glance.UnlabelledField(title)],
        ),
      )
    | // `content |> func(title: title)`
      glance.BinaryOperator(
        location: call_span,
        name: glance.Pipe,
        left: _snapshot_content,
        right: glance.Call(
          location: _,
          function:,
          arguments: [glance.LabelledField("title", _location, title)],
        ),
      )
    | // `title |> func(content, title: _)`
      glance.BinaryOperator(
        location: call_span,
        name: glance.Pipe,
        left: title,
        right: glance.FnCapture(
          location: _,
          function:,
          arguments_before: [_content],
          label: Some("title"),
          arguments_after: [],
        ),
      )
    | // `title |> func(title: _, content)`
      glance.BinaryOperator(
        location: call_span,
        name: glance.Pipe,
        left: title,
        right: glance.FnCapture(
          location: _,
          function:,
          arguments_before: [],
          label: Some("title"),
          arguments_after: [_content],
        ),
      )
    | // `title |> func(content, _)`
      glance.BinaryOperator(
        location: call_span,
        name: glance.Pipe,
        left: title,
        right: glance.FnCapture(
          location: _,
          function:,
          label: _,
          arguments_before: [glance.UnlabelledField(_snapshot_content)],
          arguments_after: [],
        ),
      ) ->
      // Most of the work is done, we have captured all calls that _look like_
      // they might be a call to `birdie.snap`, but now we need to make sure
      // they actually are!
      case is_snap_function(function, scope, snap_usage) {
        False -> Error(Nil)
        True ->
          Ok(SnapshotTest(
            title: expression_to_title(title),
            title_span: title.location,
            call_span:,
          ))
      }

    // Echo trivially wraps an expression, so we need to check that!
    glance.Echo(expression: Some(expression), ..) ->
      snapshot_test(snap_usage, scope, expression)

    // Everything else cannot be a call to `birdie.snap` (or it is a format
    // I've forgot about).
    _ -> Error(Nil)
  }
}

fn expression_to_title(title: glance.Expression) -> SnapshotTitle {
  case title {
    glance.String(value:, ..) -> LiteralTitle(title: value)
    glance.Echo(expression: Some(expression), ..) ->
      expression_to_title(expression)

    // If we're joining two or more literal strings those are still considered
    // literal titles, because we can tell at compile time what they will be.
    glance.BinaryOperator(name: glance.Concatenate, left:, right:, ..) ->
      case expression_to_title(left) {
        ExpressionTitle -> ExpressionTitle
        LiteralTitle(title: left) ->
          case expression_to_title(right) {
            LiteralTitle(title: right) -> LiteralTitle(title: left <> right)
            ExpressionTitle -> ExpressionTitle
          }
      }

    _ -> ExpressionTitle
  }
}

/// Returns `True` if the given function is a valid `birdie.snap` call given how
/// the function can be used.
fn is_snap_function(
  function: glance.Expression,
  scope: Scope,
  snap_usage: SnapUsage,
) -> Bool {
  case function {
    // We have an unqualified call: `name(content, title)`.
    // We must check that the name used is the name that was picked for the
    // unqualified birdie import.
    glance.Variable(name: used_snap_name, ..) ->
      case snap_usage {
        OnlyQualified(..) -> False
        QualifiedAndUnqualified(snap_name:, ..) | OnlyUnqualified(snap_name:) ->
          snap_name == used_snap_name
          && !set.contains(scope.variables, snap_name)
      }

    // We have a qualified call: `module_name.snap(content, title)`.
    // We must check that the name used is the name that was picked for the
    // birdie module when imported.
    glance.FieldAccess(
      container: glance.Variable(name: used_module_name, ..),
      label: "snap",
      ..,
    ) ->
      case snap_usage {
        OnlyUnqualified(..) -> False
        OnlyQualified(birdie_name:) | QualifiedAndUnqualified(birdie_name:, ..) ->
          used_module_name == birdie_name
          && !set.contains(scope.variables, birdie_name)
      }

    _ -> False
  }
}

/// How the `birdie.snap` function can be called in a module.
type SnapUsage {
  /// The birdie module has been imported but the snap function has not been
  /// imported as unqualified. For example:
  ///
  /// ```gleam
  /// import birdie as wibble
  /// //               ^^^^^^ birdie_name
  /// ```
  ///
  /// This means the function can only be called qualified as
  /// `birdie_name.snap`.
  OnlyQualified(birdie_name: String)

  /// The snap function has been imported as unqualified with the given name
  /// and the module itself has been given a name. For example:
  ///
  /// ```gleam
  /// import birdie.{snap as wibble} as wobble
  /// //                     ^^^^^^ snap_name
  /// //                                ^^^^^^ birdie_name
  /// ```
  ///
  /// This means the function could be called qualified as
  /// `module_name.snap_name`, or unqualified as `snap_name`!
  QualifiedAndUnqualified(birdie_name: String, snap_name: String)

  /// The birdie module itself is discarded, but the snap function is imported
  /// with the given name. For example:
  ///
  /// ```gleam
  /// import birdie.{snap as wibble} as _
  /// //                     ^^^^^^ snap_name
  ///
  /// /// import birdie.{snap} as _
  /// //                 ^^^^ snap_name
  /// ```
  ///
  /// This means the function can only be called as `snap_name` unqualified.
  OnlyUnqualified(snap_name: String)
}

/// Returns how the `birdie.snap` can be used inside the given module, returning
/// `Error(Nil)` if the function can't be used at all!
fn snap_usage(for module: glance.Module) -> Result(SnapUsage, Nil) {
  list.find_map(module.imports, fn(import_) {
    let glance.Import(module:, alias:, unqualified_values:, ..) =
      import_.definition

    // We only care about the import that is importing `birdie`, all the other
    // ones will be skipped.
    use <- bool.guard(when: module != "birdie", return: Error(Nil))

    // We then figure out what name we need to use for the `snap` function if it
    // is imported in an unqualified manner.
    let unqualified_snap_name =
      list.find_map(unqualified_values, fn(unqualified_import) {
        case unqualified_import {
          glance.UnqualifiedImport(name: "snap", alias: None) -> Ok("snap")
          glance.UnqualifiedImport(name: "snap", alias: Some(name)) -> Ok(name)

          glance.UnqualifiedImport(name: _, alias: Some(_))
          | glance.UnqualifiedImport(name: _, alias: None) -> Error(Nil)
        }
      })

    case alias, unqualified_snap_name {
      // `import birdie.{snap}`
      // `import birdie.{snap as snap_name}`
      None, Ok(snap_name) ->
        Ok(QualifiedAndUnqualified(birdie_name: "birdie", snap_name:))
      // `import birdie.{snap} as birdie_name`
      // `import birdie.{snap as snap_name} as birdie_name`
      Some(glance.Named(birdie_name)), Ok(snap_name) ->
        Ok(QualifiedAndUnqualified(birdie_name:, snap_name:))
      // `import birdie.{snap} as _`
      // `import birdie.{snap as snap_name} as _`
      Some(glance.Discarded(_)), Ok(snap_name) ->
        Ok(OnlyUnqualified(snap_name:))

      // `import birdie`
      None, Error(_) -> Ok(OnlyQualified(birdie_name: "birdie"))
      // `import birdie as _`
      Some(glance.Discarded(_)), Error(_) -> Error(Nil)
      // `import birdie as birdie_name`
      Some(glance.Named(birdie_name)), Error(_) ->
        Ok(OnlyQualified(birdie_name:))
    }
  })
}

// ---- GLANCE EXPRESSION FOLDING ----------------------------------------------

type Scope {
  Scope(variables: Set(String))
}

fn fold_statements(
  statements: List(glance.Statement),
  scope: Scope,
  acc: a,
  fun: fn(a, Scope, glance.Expression) -> a,
) -> a {
  let #(_scope, acc) =
    list.fold(over: statements, from: #(scope, acc), with: fn(acc, statement) {
      let #(scope, acc) = acc
      case statement {
        // A use expression can introduce new variables into scope.
        glance.Use(patterns:, function:, ..) -> {
          // The function on the right hand side of use doesn't see the variable
          // that it introduces, so we have to go over it before updating the
          // scope.
          let acc = fold_expression(function, scope, acc, fun)
          let scope =
            list.fold(patterns, scope, fn(scope, use_pattern) {
              update_scope_from_patterns(scope, [use_pattern.pattern])
            })
          #(scope, acc)
        }

        // An assignment can introduce variables into scope.
        glance.Assignment(pattern:, value:, ..) -> {
          // The value on the right hand side of the assignment doesn't see the
          // variable that it introduces, so we have to go over it before
          // updating the scope.
          let acc = fold_expression(value, scope, acc, fun)
          let scope = update_scope_from_patterns(scope, [pattern])
          #(scope, acc)
        }

        // Assertions and simple expression cannot introduce new variables in
        // the current scope!
        glance.Assert(location: _, expression:, message: None) -> {
          #(scope, fold_expression(expression, scope, acc, fun))
        }
        glance.Assert(location: _, expression:, message: Some(message)) -> {
          let acc = fold_expression(expression, scope, acc, fun)
          #(scope, fold_expression(message, scope, acc, fun))
        }
        glance.Expression(expression) -> {
          #(scope, fold_expression(expression, scope, acc, fun))
        }
      }
    })

  acc
}

fn fold_expression(
  expression: glance.Expression,
  scope: Scope,
  acc: a,
  fun: fn(a, Scope, glance.Expression) -> a,
) -> a {
  let acc = fun(acc, scope, expression)
  case expression {
    glance.Echo(expression: Some(expression), message: None, ..)
    | glance.Echo(expression: None, message: Some(expression), ..) ->
      fold_expression(expression, scope, acc, fun)
    glance.Echo(expression: Some(expression), message: Some(message), ..) -> {
      let acc = fold_expression(expression, scope, acc, fun)
      fold_expression(message, scope, acc, fun)
    }

    glance.NegateInt(value: expression, ..)
    | glance.NegateBool(value: expression, ..)
    | glance.FieldAccess(container: expression, ..)
    | glance.TupleIndex(tuple: expression, ..) ->
      fold_expression(expression, scope, acc, fun)

    glance.Block(statements:, ..) ->
      fold_statements(statements, scope, acc, fun)

    glance.Tuple(elements:, ..) | glance.List(elements:, rest: None, ..) ->
      fold_expressions(elements, scope, acc, fun)

    glance.List(elements:, rest: Some(rest), ..) -> {
      let acc = fold_expressions(elements, scope, acc, fun)
      fold_expression(rest, scope, acc, fun)
    }

    glance.Fn(body: statements, arguments:, ..) -> {
      let scope =
        list.fold(arguments, scope, fn(scope, argument) {
          case argument.name {
            glance.Discarded(_) -> scope
            glance.Named(name) -> Scope(set.insert(scope.variables, name))
          }
        })
      fold_statements(statements, scope, acc, fun)
    }

    glance.RecordUpdate(record:, fields:, ..) -> {
      let acc = fold_expression(record, scope, acc, fun)
      list.fold(over: fields, from: acc, with: fn(acc, field) {
        case field.item {
          Some(item) -> fold_expression(item, scope, acc, fun)
          None -> acc
        }
      })
    }

    glance.Call(function:, arguments:, ..) -> {
      let acc = fold_expression(function, scope, acc, fun)
      fold_fields(arguments, scope, acc, fun)
    }

    glance.FnCapture(function:, arguments_before:, arguments_after:, ..) -> {
      let acc = fold_expression(function, scope, acc, fun)
      let acc = fold_fields(arguments_before, scope, acc, fun)
      fold_fields(arguments_after, scope, acc, fun)
    }

    glance.Case(subjects:, clauses:, ..) -> {
      let acc = fold_expressions(subjects, scope, acc, fun)
      fold_clauses(clauses, scope, acc, fun)
    }

    glance.BinaryOperator(left:, right:, ..) -> {
      let acc = fold_expression(left, scope, acc, fun)
      fold_expression(right, scope, acc, fun)
    }

    glance.Panic(message: Some(expression), ..)
    | glance.Todo(message: Some(expression), ..) ->
      fold_expression(expression, scope, acc, fun)

    // We can't find any `birdie.snap` call here for sure.
    glance.Panic(message: None, ..)
    | glance.Todo(message: None, ..)
    | glance.BitString(..)
    | glance.Int(..)
    | glance.Float(..)
    | glance.String(..)
    | glance.Variable(..)
    | glance.Echo(expression: None, message: None, ..) -> acc
  }
}

fn fold_fields(
  fields: List(glance.Field(glance.Expression)),
  scope: Scope,
  acc: a,
  fun: fn(a, Scope, glance.Expression) -> a,
) -> a {
  list.fold(over: fields, from: acc, with: fn(acc, field) {
    case field {
      glance.LabelledField(item:, ..) | glance.UnlabelledField(item:) ->
        fold_expression(item, scope, acc, fun)
      glance.ShorthandField(..) -> acc
    }
  })
}

fn fold_clauses(
  clauses: List(glance.Clause),
  scope: Scope,
  acc: a,
  fun: fn(a, Scope, glance.Expression) -> a,
) -> a {
  list.fold(over: clauses, from: acc, with: fn(acc, clause) {
    case clause {
      glance.Clause(patterns:, guard: None, body:) -> {
        let scope = list.fold(patterns, scope, update_scope_from_patterns)
        fold_expression(body, scope, acc, fun)
      }
      glance.Clause(patterns:, guard: Some(guard), body:) -> {
        let scope = list.fold(patterns, scope, update_scope_from_patterns)
        let acc = fold_expression(guard, scope, acc, fun)
        fold_expression(body, scope, acc, fun)
      }
    }
  })
}

fn fold_expressions(
  expressions: List(glance.Expression),
  scope: Scope,
  acc: a,
  fun: fn(a, Scope, glance.Expression) -> a,
) -> a {
  list.fold(expressions, acc, fn(acc, expression) {
    fold_expression(expression, scope, acc, fun)
  })
}

fn update_scope_from_patterns(
  scope: Scope,
  patterns: List(glance.Pattern),
) -> Scope {
  Scope(list.fold(patterns, scope.variables, pattern_variables))
}

fn pattern_variables(acc: Set(String), pattern: glance.Pattern) -> Set(String) {
  case pattern {
    glance.PatternInt(..) -> acc
    glance.PatternFloat(..) -> acc
    glance.PatternString(..) -> acc
    glance.PatternDiscard(..) -> acc
    glance.PatternVariable(name:, ..) -> set.insert(acc, name)

    glance.PatternTuple(elements:, ..) ->
      list.fold(elements, acc, pattern_variables)

    glance.PatternList(elements:, tail: None, ..) ->
      list.fold(elements, acc, pattern_variables)
    glance.PatternList(elements:, tail: Some(tail), ..) -> {
      let acc = list.fold(elements, acc, pattern_variables)
      pattern_variables(acc, tail)
    }

    glance.PatternAssignment(pattern:, name:, ..) -> {
      let acc = set.insert(acc, name)
      pattern_variables(acc, pattern)
    }

    glance.PatternConcatenate(prefix_name:, rest_name:, ..) -> {
      let acc = case prefix_name {
        Some(glance.Discarded(_)) | None -> acc
        Some(glance.Named(name)) -> set.insert(acc, name)
      }
      case rest_name {
        glance.Named(name) -> set.insert(acc, name)
        glance.Discarded(_) -> acc
      }
    }

    glance.PatternBitString(segments:, ..) ->
      list.fold(segments, acc, fn(acc, segment) {
        pattern_variables(acc, segment.0)
      })

    glance.PatternVariant(arguments:, ..) ->
      list.fold(arguments, acc, fn(acc, argument) {
        case argument {
          glance.ShorthandField(label:, ..) -> set.insert(acc, label)
          glance.LabelledField(item:, ..) | glance.UnlabelledField(item:) ->
            pattern_variables(acc, item)
        }
      })
  }
}
