import streams
import sugar

import results

type
  ParsePosition = tuple
    column, line: int

  ParseError = tuple
    ## A `ParseError` contains both what was expected by the `Parser`, and
    ## what was actually found by it (the unexpected `string`).
    unexpected: string
    expected: seq[string]

  ParseState* = ref object
    ## A `ParseState` keeps track of the `Stream` and where we are at it.
    stream: Stream
    position, lastPosition: ParsePosition
    atNewLine: bool

  ParseResult*[T] = Result[T,ParseError]
    ## A `ParseResult` of type `T` is a `Result` object with either a parsed
    ## object of type `T` or a `ParseError`.

  Parser*[T] = ParseState -> ParseResult[T]
    ## A `Parser` for a type `T` is a function that receives a `ParseState`
    ## and gives back a `ParseResult` of type `T`.


proc atEnd*(s: ParseState): bool {.inline.} =
  ## Checks if more data can be read from `s`.
  ## Returns `true` if all data has been read.
  s.stream.atEnd

proc peekChar*(s: ParseState): char {.inline.} =
  ## Peeks a char from the `ParseState` `s`.
  ## Raises `IOError` if an error occurred.
  ## Returns '\0' as an EOF marker.
  s.stream.peekChar


proc stepBack*(s: ParseState) {.inline.} =
  s.position = s.lastPosition
  s.stream.setPosition(s.stream.getPosition - 1)


template getPosition*(s: ParseState): int =
  ## Retrieves the current position in the `ParseState` `s`.
  s.stream.getPosition


template readChar*(s: ParseState): char =
  ## Reads a char from the `ParseState` `s`.
  ## Raises `IOError` if an error occurred.
  ## Returns '\0' as an EOF marker.
  s.lastPosition = s.position
  let c = s.stream.readChar
  if not s.atNewLine:
    s.position.column += 1
    if c == '\n':
      s.atNewLine = true
  else:
    if c != '\n':
      # TODO: is it "\r\n" or "\n\r"? And don't forget to test this!
      if c != '\r':
        # We just consumed the first char of a new line
        s.position = (column: 1, line: s.position.line + 1)
        s.atNewLine = false
      else:
        # '\r' is still part of the current line
        s.position.column += 1
    else:
      # We're at the end of an empty line
      s.position = (column: 0, line: s.position.line + 1)
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
  parser newParseState(x)

template debugParse*[T](parser: Parser[T], x: auto): (ParseResult[T], int, int, int) =
  let s = newParseState(x)
  # TODO: for debugging purposes, it is more useful to return the rest of the
  # input, instead of only its position.
  (parser s, s.stream.getPosition, s.position.line, s.position.column)
