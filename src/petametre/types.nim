import streams
import strutils
import sugar

import results

type
  ParsePosition = tuple
    ## A `ParsePosition` holds the current column and line numbers, as well
    ## as the position of the start of the current line.
    column, line, currentLine: int

  ParseError = tuple
    ## A `ParseError` contains what was expected by the `Parser`, what was
    ## actually found by it (the unexpected `string`) and the current
    ## `ParseState`.
    unexpected: string
    expected: seq[string]
    state: ParseState

  ParseState* = ref object
    ## A `ParseState` keeps track of the `Stream` and where we are at it.
    stream: Stream
    position, lastPosition: ParsePosition  # TODO: do we really need lastPosition?
    atNewLine: bool  # TODO: should atNewLine be in ParsePosition?

  ParseResult*[T] = Result[T,ParseError]
    ## A `ParseResult` of type `T` is a `Result` object with either a parsed
    ## object of type `T` or a `ParseError`.

  Parser*[T] = ParseState -> ParseResult[T]
    ## A `Parser` for a type `T` is a function that receives a `ParseState`
    ## and gives back a `ParseResult` of type `T`.

func failure*[T](unexpected: string, expected: seq[string], state: ParseState): ParseResult[T] {.inline.} =
  ## Stop parsing and report a `ParseError`.
  ParseResult[T].err(
    (unexpected, expected, state)
  )

func failure*[T](res: ParseResult[auto]): ParseResult[T] {.inline.} =
  ## Stop parsing and report the `ParseError` of a `ParseResult`.
  ParseResult[T].err(res.error)


proc getCurrentLine(state: ParseState): string {.inline.} =
  ## Get the current line as a `string`.
  let position = state.stream.getPosition
  state.stream.setPosition(state.position.currentLine)
  discard state.stream.readLine(result)
  state.stream.setPosition(position)


proc `$`*[T](res: ParseResult[T]): string {.inline.} =
  ## Represent a `ParseResult` as a `string`. Mostly used to print errors.
  if res.isErr:
    let
      error = res.error
      state = error.state

      column = state.position.column
      lineStr = $state.position.line
      margin = indent("|", len(lineStr) + 1)

      expectedItems = if len(error.expected) < 2:
        # TODO: what to do with zero elements?
        join(error.expected, ", ")
      elif len(error.expected) == 2:
        error.expected[0] & " or " & error.expected[1]
      else:
        join(error.expected[0..^2], ", ") & ", or " & error.expected[^1]

      positionInfo = lineStr & ':' & $column & ":(" & $state.stream.getPosition & "):"
      offendingLine = getCurrentLine(state)
      markingCaret = indent("^", column)
      unexpectedInfo = "unexpected " & error.unexpected
      expectingInfo = "expecting " & expectedItems

    positionInfo   &                         '\n' &
    margin         &                         '\n' &
    lineStr        & " | " & offendingLine & '\n' &
    margin         & ' '   & markingCaret  & '\n' &
    unexpectedInfo &                         '\n' &
    expectingInfo
  else:
    debugEcho "asdf"
    "Got " & $res.get  # TODO: this is temporary


proc atEnd*(state: ParseState): bool {.inline.} =
  ## Checks if more data can be read from `s`.
  ## Returns `true` if all data has been read.
  state.stream.atEnd

proc peekChar*(state: ParseState): char {.inline.} =
  ## Peeks a char from the `ParseState` `s`.
  ## Raises `IOError` if an error occurred.
  ## Returns '\0' as an EOF marker.
  state.stream.peekChar


proc stepBack*(state: ParseState) {.inline.} =
  ## Go a step back.
  state.position = state.lastPosition
  state.stream.setPosition(state.stream.getPosition - 1)


template getPosition*(state: ParseState): int =
  ## Retrieves the current position in the `ParseState` `s`.
  state.stream.getPosition


template readChar*(state: ParseState): char =
  ## Reads a char from the `ParseState` `s`.
  ## Raises `IOError` if an error occurred.
  ## Returns '\0' as an EOF marker.
  state.lastPosition = state.position
  let c = state.stream.readChar
  if not state.atNewLine:
    state.position.column += 1
    if c == '\n':
      state.atNewLine = true
  else:
    if c != '\n':
      # TODO: is it "\r\n" or "\n\r"? And don't forget to test this!
      if c != '\r':
        # We just consumed the first char of a new line
        state.position = (column: 1, line: state.position.line + 1, currentLine: state.stream.getPosition - 1)
        state.atNewLine = false
      else:
        # '\r' is still part of the current line
        state.position.column += 1
    else:
      # We're at the end of an empty line
      state.position = (column: 0, line: state.position.line + 1, currentLine: state.position.currentLine)
  c


# TODO: support working with files through openFileStream.
func newParseState(s: Stream): ParseState {.inline.} =
  ## Creates a new `ParseState` from the stream `s`.
  ParseState(stream: s)

# TODO: should we call close on the stream?
template newParseState(s: string): ParseState =
  ## Creates a new `ParseState` from the string `s`.
  newParseState newStringStream(s)


template parse*[T](parser: Parser[T], x: auto): ParseResult[T] =
  ## Apply a `Parser` to `x`.
  parser newParseState(x)

proc debugParse*[T](parser: Parser[T], x: auto): string {.inline.} =
  let
    state = newParseState(x)
    res = parser(state)
  # TODO: for debugging purposes, it is more useful to return the rest of the
  # input, instead of only its position.
  if res.isOk:
    $(res.get, state.stream.getPosition, state.position.line, state.position.column)
  else:
    $((unexpected: res.error.unexpected, expected: res.error.expected), state.stream.getPosition, state.position.line, state.position.column)

proc debugParse*(parser: Parser[void], x: auto): string {.inline.} =
  let
    state = newParseState(x)
    res = parser(state)
  # TODO: for debugging purposes, it is more useful to return the rest of the
  # input, instead of only its position.
  if res.isOk:
    $(state.stream.getPosition, state.position.line, state.position.column)
  else:
    $((unexpected: res.error.unexpected, expected: res.error.expected), state.stream.getPosition, state.position.line, state.position.column)
