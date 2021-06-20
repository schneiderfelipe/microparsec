import streams
import sugar

import results

type
  ParseState* = tuple
    ## A `ParseState` keeps track of the `Stream` and where we are at it.
    stream: Stream

  ParseError = tuple
    ## A `ParseError` contains both what was expected by the `Parser`, and
    ## what was actually found by it (the unexpected `string`).
    unexpected: string
    expected: seq[string]

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

# This is NOT exported and I like that.
proc setPosition(s: ParseState, pos: int) {.inline.} =
  ## Sets the position `pos` of the `ParseState` `s`.
  s.stream.setPosition(pos)

proc getPosition*(s: ParseState): int {.inline.} =
  ## Retrieves the current position in the `ParseState` `s`.
  s.stream.getPosition

proc stepBack*(s: ParseState) {.inline.} =
  s.setPosition(s.getPosition - 1)

proc readChar*(s: ParseState): char {.inline.} =
  ## Reads a char from the `ParseState` `s`.
  ## Raises `IOError` if an error occurred.
  ## Returns '\0' as an EOF marker.
  s.stream.readChar

proc peekChar*(s: ParseState): char {.inline.} =
  ## Peeks a char from the `ParseState` `s`.
  ## Raises `IOError` if an error occurred.
  ## Returns '\0' as an EOF marker.
  s.stream.peekChar


# TODO: support working with files through openFileStream.
func newParseState(s: Stream): ParseState {.inline.} =
  ## Creates a new `ParseState` from the stream `s`.
  (stream: s)

# TODO: should we call close on the stream?
template newParseState(s: string): ParseState =
  ## Creates a new `ParseState` from the string `s`.
  newParseState newStringStream(s)


template parse*[T](parser: Parser[T], x: auto): ParseResult[T] =
  parser newParseState(x)

template debugParse*[T](parser: Parser[T], x: auto): (ParseResult[T], int) =
  let s = newParseState(x)
  # TODO: for debugging purposes, it is more useful to return the rest of the
  # input, instead of only its position.
  (parser s, s.getPosition)
