import birdie/internal/analyser.{
  type Analyser, ExpressionTitle, LiteralTitle, SnapshotTest,
}
import birdie/internal/position.{type Position, Position}
import filepath
import glance
import gleam/bit_array
import gleam/dynamic/decode.{type Decoder, type Dynamic}
import gleam/int
import gleam/io
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleam/uri.{type Uri}
import justin
import simplifile

type Error {
  CannotReadInitialiseRequest(reason: RequestError)
  RequestBeforeInitialise(request: Request)
  InvalidWorkspaceFolders(workspace_folders: List(Uri))
}

type RequestError {
  CannotReadMessageContentLength(reason: Dynamic)
  InvalidMessageContentLength(content_length: BitArray)
  CannotReadMessageContent(reason: Dynamic)
  MissingEmptyLine
  CannotDecodeRequest(message: BitArray, reason: json.DecodeError)
}

type Server {
  Server(reader: Reader, analyser: Analyser, shut_down: Bool, project_root: Uri)
}

pub fn start() {
  case initialise() {
    // In case we can't initialise we just close the connection to the client.
    Error(_) -> Nil
    // Otherwise we enter the main loop, where we wait for requests.
    Ok(server) -> main_loop(server)
  }
}

fn initialise() -> Result(Server, Error) {
  let reader = start_reader()

  case read_request(reader) {
    Error(reason) -> Error(CannotReadInitialiseRequest(reason:))
    Ok(#(reader, Initialise(id:, workspace_folders: [project_root]))) -> {
      send_response(id, Initialised)
      Ok(Server(
        reader:,
        analyser: analyser.new(),
        shut_down: False,
        project_root:,
      ))
    }

    Ok(#(_, Initialise(workspace_folders:, ..))) ->
      Error(InvalidWorkspaceFolders(workspace_folders:))

    Ok(#(_, request)) -> Error(RequestBeforeInitialise(request))
  }
}

fn main_loop(server: Server) {
  case read_request(server.reader) {
    Error(_error) -> main_loop(server)

    Ok(#(reader, request)) -> {
      let server = Server(..server, reader:)
      let server = handle_request(server, request)
      main_loop(server)
    }
  }
}

fn handle_request(server: Server, request: Request) -> Server {
  case request {
    Shutdown(id:) -> {
      send_response(id, ShuttingDown)
      server
    }

    Exit if server.shut_down -> exit(0)
    Exit -> exit(1)

    // No need to do anything on initialise, we've dealt with that earlier
    Initialise(..) -> server

    // If a document changes we need to compile it again!
    // TODO)) Might need to debounce change events a little bit...
    DocumentChanged(document: Document(path:, source:)) -> {
      let analyser =
        analyser.remove_module(server.analyser, path)
        |> analyser.analyse(analyser.Module(path:, source:))
      Server(..server, analyser:)
    }

    // If a document is opened, we need to analyse it, the source is provided in
    // the notification so we don't have to read it from disk.
    DocumentOpened(document: Document(path:, source:)) -> {
      let analyser =
        analyser.remove_module(server.analyser, path)
        |> analyser.analyse(analyser.Module(path:, source:))

      Server(..server, analyser:)
    }

    // If a document is closed we remove it from the cache, so we can keep
    // memory consumption down.
    // This comes at a little price of analysing the module again if it's opened
    // once more.
    DocumentClosed(document:) -> {
      let analyser = analyser.remove_module(server.analyser, document)
      Server(..server, analyser:)
    }

    // Someone is asking for an hover! Time to show the content of the snapshot.
    Hover(id:, document:, position:) ->
      case analyser.find_test(server.analyser, document, position) {
        Error(_) | Ok(#(_, SnapshotTest(title: ExpressionTitle, ..))) -> {
          send_response(id, HoverResponse(None))
          server
        }
        // If the title is not a literal expression there's nothing we can show
        // as we can't figure out what name the snapshot file has :(
        Ok(#(
          line_numbers,
          SnapshotTest(title: LiteralTitle(title), call_span:, ..),
        )) -> {
          case read_snapshot_content(server, title) {
            Error(_) -> send_response(id, HoverResponse(None))
            Ok(#(kind, content)) -> {
              let heading = case kind {
                New -> "*new snapshot*"
                Accepted -> "*accepted snapshot*"
              }
              let content = heading <> "\n```\n" <> content <> "```"
              let range = span_to_range(line_numbers, call_span)
              let result = HoverResponse(Some(#(range, content)))
              send_response(id, result)
            }
          }

          server
        }
      }
  }
}

fn span_to_range(line_numbers: position.Map, call_span: glance.Span) -> Range {
  Range(
    start: position.from_byte_index(line_numbers, call_span.start),
    end: position.from_byte_index(line_numbers, call_span.end),
  )
}

/// A little wrapper type to represent accepted or new snapshots, this doesn't
/// need all the bells and whistles of the one defined in the birdie module.
/// Trying to abstract over this and only use one time would be a waste of time!
type SnapshotKind {
  New
  Accepted
}

fn read_snapshot_content(
  server: Server,
  titled title: String,
) -> Result(#(SnapshotKind, String), Nil) {
  let path =
    server.project_root.path
    |> filepath.join("birdie_snapshots")
    |> filepath.join(file_name(title))

  let trim_content = fn(content) {
    case string.split(content, on: "\n---\n") {
      [_, content] -> content
      _ -> content
    }
  }

  case simplifile.read(path <> ".accepted") {
    Ok(content) -> Ok(#(Accepted, trim_content(content)))
    Error(simplifile.Enoent) ->
      case simplifile.read(path <> ".new") {
        Ok(content) -> Ok(#(New, trim_content(content)))
        Error(_) -> Error(Nil)
      }
    Error(_) -> Error(Nil)
  }
}

/// TODO)) This should be shared with the birdie module. If that changes this
/// has to change as well!
fn file_name(title: String) -> String {
  string.replace(each: "/", with: " ", in: title)
  |> string.replace(each: "\\", with: " ")
  |> string.replace(each: "\n", with: " ")
  |> string.replace(each: "\t", with: " ")
  |> string.replace(each: "\r", with: " ")
  |> string.replace(each: ".", with: " ")
  |> string.replace(each: ":", with: " ")
  |> justin.snake_case
}

// ---- LSP REQUESTS -----------------------------------------------------------

type Request {
  Initialise(id: Id, workspace_folders: List(Uri))
  Hover(id: Id, document: Uri, position: Position)
  Shutdown(id: Id)
  Exit
  DocumentOpened(document: Document)
  DocumentChanged(document: Document)
  DocumentClosed(document: Uri)
}

type Id {
  StringId(String)
  IntId(Int)
}

type Range {
  Range(start: Position, end: Position)
}

type Document {
  Document(path: Uri, source: String)
}

fn read_request(reader: Reader) -> Result(#(Reader, Request), RequestError) {
  use #(reader, content_length) <- result.try(read_content_length(reader))
  use #(reader, _) <- result.try(
    read_line(reader) |> result.replace_error(MissingEmptyLine),
  )

  case read_bytes(reader, content_length) {
    Error(reason) -> Error(CannotReadMessageContent(reason:))
    Ok(#(reader, message)) -> {
      use request <- result.try(parse_request(message))
      Ok(#(reader, request))
    }
  }
}

fn read_content_length(reader: Reader) -> Result(#(Reader, Int), RequestError) {
  case read_line(reader) {
    Ok(#(reader, <<"Content-Length: ", number:bits>>)) ->
      case result.try(bit_array.to_string(number), int.parse) {
        Ok(content_length) -> Ok(#(reader, content_length))
        Error(_) -> Error(InvalidMessageContentLength(content_length: number))
      }

    Ok(#(_reader, content_length)) ->
      Error(InvalidMessageContentLength(content_length:))
    Error(reason) -> Error(CannotReadMessageContentLength(reason:))
  }
}

fn parse_request(message: BitArray) -> Result(Request, RequestError) {
  json.parse_bits(message, request_decoder())
  |> result.map_error(CannotDecodeRequest(message:, reason: _))
}

fn id_decoder() -> Decoder(Id) {
  decode.one_of(decode.int |> decode.map(IntId), [
    decode.string |> decode.map(StringId),
  ])
  |> decode.collapse_errors("Id")
}

fn request_decoder() -> Decoder(Request) {
  use method <- decode.field("method", decode.string)
  case method {
    "shutdown" -> shutdown_request_decoder()
    "initialize" -> initialise_request_decoder()
    "exit" -> exit_request_decoder()
    "textDocument/hover" -> hover_request_decoder()
    "textDocument/didOpen" -> decode.at(["params"], did_open_request_decoder())
    "textDocument/didChange" ->
      decode.at(["params"], did_change_request_decoder())
    "textDocument/didClose" ->
      decode.at(["params"], did_close_request_decoder())
    method ->
      decode.failure(Initialise(IntId(0), []), "RequestMethod")
      |> decode.map_errors(fn(errors) {
        list.map(errors, fn(error) {
          decode.DecodeError(..error, found: method)
        })
      })
  }
}

fn exit_request_decoder() -> Decoder(Request) {
  decode.success(Exit)
}

fn shutdown_request_decoder() -> Decoder(Request) {
  use id <- decode.field("id", id_decoder())
  decode.success(Shutdown(id:))
}

fn initialise_request_decoder() -> Decoder(Request) {
  use id <- decode.field("id", id_decoder())
  decode.at(["params"], {
    use workspace_folders <- decode.optional_field(
      "workspaceFolders",
      [],
      decode.list(workspace_folder_decoder()),
    )
    decode.success(Initialise(id:, workspace_folders:))
  })
}

fn workspace_folder_decoder() -> Decoder(Uri) {
  use uri <- decode.field("uri", uri_decoder())
  decode.success(uri)
}

fn hover_request_decoder() -> Decoder(Request) {
  use id <- decode.field("id", id_decoder())
  decode.at(["params"], {
    use document <- decode.subfield(["textDocument", "uri"], uri_decoder())
    use position <- decode.field("position", position_decoder())
    decode.success(Hover(id:, document:, position:))
  })
}

fn did_open_request_decoder() -> Decoder(Request) {
  decode.at(["textDocument"], {
    use path <- decode.field("uri", uri_decoder())
    use source <- decode.field("text", decode.string)
    decode.success(DocumentOpened(Document(path:, source:)))
  })
}

fn did_close_request_decoder() -> Decoder(Request) {
  use document <- decode.subfield(["textDocument", "uri"], uri_decoder())
  decode.success(DocumentClosed(document:))
}

fn did_change_request_decoder() -> Decoder(Request) {
  use path <- decode.subfield(["textDocument", "uri"], uri_decoder())
  use source <- decode.field(
    "contentChanges",
    decode.at([0], {
      use string <- decode.field("text", decode.string)
      decode.success(string)
    }),
  )
  decode.success(DocumentChanged(document: Document(path:, source:)))
}

fn position_decoder() -> Decoder(Position) {
  use line <- decode.field("line", decode.int)
  use character <- decode.field("character", decode.int)
  decode.success(Position(line:, character:))
}

fn uri_decoder() -> Decoder(Uri) {
  use string <- decode.then(decode.string)
  case uri.parse(string) {
    Ok(uri) -> decode.success(uri)
    Error(_) -> decode.failure(uri.empty, "Uri")
  }
}

// ---- LSP RESPONSES ----------------------------------------------------------

type Response {
  Response(id: Id, result: Result(ResponseResult, ResponseError))
}

type ResponseResult {
  Initialised
  HoverResponse(content: option.Option(#(Range, String)))
  ShuttingDown
}

type ResponseError

fn send_response(id: Id, result: ResponseResult) -> Nil {
  Response(id:, result: Ok(result))
  |> response_to_json
  |> json.to_string
  |> encode_message
  |> io.print
}

fn encode_message(message: String) -> String {
  let message_bytes = string.byte_size(message)
  "Content-Length: " <> int.to_string(message_bytes) <> "\r\n\r\n" <> message
}

fn response_to_json(response: Response) -> Json {
  let Response(id:, result:) = response
  json.object([
    #("id", id_to_json(id)),
    #("jsonrpc", json.string("2.0")),
    case result {
      Ok(result) -> #("result", response_result_to_json(result))

      // For now we don't have any errors!
      Error(_error) -> #("error", json.null())
    },
  ])
}

fn id_to_json(id: Id) -> Json {
  case id {
    StringId(id) -> json.string(id)
    IntId(id) -> json.int(id)
  }
}

fn response_result_to_json(result: ResponseResult) -> Json {
  case result {
    Initialised ->
      json.object([
        #(
          "capabilities",
          json.object([
            #(
              "textDocumentSync",
              json.object([
                #("openClose", json.bool(True)),
                #("change", json.int(1)),
              ]),
            ),
            #("hoverProvider", json.bool(True)),
            #("implementationProvider", json.bool(True)),
          ]),
        ),
      ])

    HoverResponse(content: None) -> json.null()
    HoverResponse(content: Some(#(range, content))) ->
      json.object([
        #("range", range_to_json(range)),
        #(
          "contents",
          json.object([
            #("kind", json.string("markdown")),
            #("value", json.string(content)),
          ]),
        ),
      ])

    ShuttingDown -> json.null()
  }
}

fn range_to_json(range: Range) -> Json {
  let Range(start:, end:) = range
  json.object([
    #("start", position_to_json(start)),
    #("end", position_to_json(end)),
  ])
}

fn position_to_json(position: Position) -> Json {
  let Position(line:, character:) = position
  json.object([
    #("line", json.int(line)),
    #("character", json.int(character)),
  ])
}

// ---- LSP NOTIFICATIONS ------------------------------------------------------

// type Notification {
//   Log(level: LogLevel, message: String)
// }
//
// type LogLevel {
//   Info
//   Erro
// }
//
// fn log(level: LogLevel, message: String) -> Nil {
//   Log(level:, message:)
//   |> send_notification
// }
//
// fn send_notification(notification: Notification) -> Nil {
//   notification_to_json(notification)
//   |> json.to_string
//   |> encode_message
//   |> io.print
// }
//
// fn notification_to_json(notification: Notification) -> Json {
//   let #(method, params) = case notification {
//     Log(level:, message:) -> #(
//       "window/logMessage",
//       json.object([
//         #("type", json.int(log_level_to_int(level))),
//         #("message", json.string(message)),
//       ]),
//     )
//   }
//
//   json.object([
//     #("method", json.string(method)),
//     #("params", params),
//   ])
// }
//
// fn log_level_to_int(level: LogLevel) -> Int {
//   case level {
//     Info -> 3
//     Erro -> 1
//   }
// }

// ---- FFI FOR READING STDIN --------------------------------------------------

type Reader

@external(erlang, "birdie_lsp_ffi", "start")
fn start_reader() -> Reader

@external(erlang, "birdie_lsp_ffi", "read_line")
fn read_line(reader: Reader) -> Result(#(Reader, BitArray), Dynamic)

@external(erlang, "birdie_lsp_ffi", "read_bytes")
fn read_bytes(
  reader: Reader,
  bytes: Int,
) -> Result(#(Reader, BitArray), Dynamic)

@external(erlang, "erlang", "halt")
fn exit(int: Int) -> a
