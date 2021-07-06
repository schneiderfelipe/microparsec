import streams
export atEnd, getPosition, setPosition

import strutils
import sugar

import results

type
  ParsePosition = tuple
    ## A `ParsePosition` holds the current column and line numbers, as well
    ## as the position of the start of the current line.
    column, line, currentLine: int
    atNewLine: bool

  ParseError = tuple
    ## A `ParseError` contains what was expected by the `Parser`, what was
    ## actually found by it (the unexpected `string`), the current
    ## `ParseState` and an extra message.
    unexpected: string
    expected: seq[string]
    state: ParseState
    message: string

  ParseState* = ref object
    ## A `ParseState` keeps track of the `Stream` and where we are at it.
    stream: Stream
    position, lastPosition: ParsePosition

  ParseResult*[T] = Result[T, ParseError]
    ## A `ParseResult` of type `T` is a `Result` object with either a parsed
    ## object of type `T` or a `ParseError`.

  Parser*[T] = ParseState -> ParseResult[T]
    ## A `Parser` for a type `T` is a function that receives a `ParseState`
    ## and gives back a `ParseResult` of type `T`.

func fail*[T](unexpected: string, expected: openArray[string],
    state: ParseState, message = ""): ParseResult[T] {.inline.} =
  ## Stop parsing and report a `ParseError`.
  ParseResult[T].err (unexpected, @expected, state, message)

func fail*[T](res: ParseResult[auto]): ParseResult[T] {.inline.} =
  ## Stop parsing and report the `ParseError` of a `ParseResult`.
  ParseResult[T].err res.error


proc getCurrentLine(state: ParseState): string {.inline.} =
  ## Get the current line as a `string`.
  let position = state.stream.getPosition
  state.stream.setPosition state.position.currentLine
  discard state.stream.readLine result
  state.stream.setPosition position


proc `$`*[T](res: ParseResult[T]): string {.inline.} =
  ## Represent a `ParseResult` as a `string`. Mostly used to print errors.
  if res.isErr:
    let
      error = res.error
      state = error.state

      heading = if len(error.message) > 0:
        "Failed reading: " & error.message & "\n\n"
      else:
        ""

      column = state.position.column
      lineStr = $state.position.line
      margin = indent("|", len(lineStr) + 1)

      expectedItems = if len(error.expected) > 0:
        if len(error.expected) < 2:
          join(error.expected, ", ")
        elif len(error.expected) == 2:
          error.expected[0] & " or " & error.expected[1]
        else:
          join(error.expected[0..^2], ", ") & ", or " & error.expected[^1]
      else:
        ""

      positionInfo = lineStr & ':' & $column & ":(" &
          $state.stream.getPosition & "):"
      offendingLine = getCurrentLine state
      markingCaret = indent("^", column)
      unexpectedInfo = "unexpected " & error.unexpected
      expectingInfo = if len(expectedItems) > 0:
        "\nexpecting " & expectedItems
      else:
        ""

    return heading &
    positionInfo & '\n' &
    margin & '\n' &
    lineStr & " | " & offendingLine & '\n' &
    margin & ' ' & markingCaret & '\n' &
    unexpectedInfo &
    expectingInfo

  return "Got " & $res.get


template atEnd*(state: ParseState): bool =
  ## Checks if more data can be read from `state`.
  ## Returns `true` if all data has been read.
  state.stream.atEnd

proc peekChar*(state: ParseState): char {.inline.} =
  ## Peeks a char from the `ParseState` `state`.
  ## Raises `IOError` if an error occurred.
  ## Returns '\0' as an EOF marker.
  state.stream.peekChar


template stepBack*(state: ParseState) =
  ## Go a step back.
  state.position = state.lastPosition
  state.stream.setPosition(state.stream.getPosition - 1)


template getPosition*(state: ParseState): int =
  ## Retrieves the current position in the `ParseState` `state`.
  state.stream.getPosition


template readChar*(state: ParseState): char =
  ## Reads a char from the `ParseState` `state`.
  ## Raises `IOError` if an error occurred.
  ## Returns '\0' as an EOF marker.
  state.lastPosition = state.position
  let c = state.stream.readChar
  if not state.position.atNewLine:
    state.position.column += 1
    if c == '\n':
      state.position.atNewLine = true
  else:
    if c != '\n':
      if c != '\r':
        # We just consumed the first char of a new line
        state.position.column = 1
        state.position.line += 1
        state.position.currentLine = state.stream.getPosition - 1
        state.position.atNewLine = false
      else:
        # '\r' is still part of the current line
        state.position.column += 1
    else:
      # We're at the end of an empty line
      state.position.column = 0
      state.position.line += 1
  c

proc readLastStr*(state: ParseState, length: int): string {.inline.} =
  ## Reads a string of length `length` from the `ParseState` `state` *ending*
  ## at the current position. Raises `IOError` if an error occurred.
  state.stream.setPosition state.stream.getPosition - length
  result = state.stream.readStr(length)

proc readStr*(state: ParseState, length: int): string {.inline.} =
  ## Reads a string of length `length` from the `ParseState` `state`
  ## *starting* at the current position. Raises `IOError` if an error occurred.
  ##
  ## **Note**: This function is experimental.
  for _ in 0..<length:
    result.add state.readChar

func newParseState*(s: Stream): ParseState {.inline.} =
  ## Creates a new `ParseState` from the stream `s`.
  ParseState(stream: s)

template newParseState*(s: string): ParseState =
  ## Creates a new `ParseState` from the string `s`.
  newParseState newStringStream s


template parse*[T](parser: Parser[T], x: auto): ParseResult[T] =
  ## Apply a `Parser` to `x`.
  parser newParseState x

template parse*[T](parser: Parser[T], x: ParseState): ParseResult[T] =
  ## Apply a `Parser` to `x`.
  parser x


proc debugParse*[T](parser: Parser[T], x: auto, withPosition = false): string {.inline.} =
  let
    state = newParseState x
    res = parser state
  result &= '('
  if res.isOk:
    when T isnot void:
      result.addQuoted res.get
      result &= ", "
    result.addQuoted state.stream.readAll
    if withPosition:
      result &= ", " & $state.getPosition & ", " & $state.position.line & ", " &
          $state.position.column
  else:
    result &= "unexpected: "
    result.addQuoted res.error.unexpected
    result &= ", expected: " & $res.error.expected
  result &= ')'
