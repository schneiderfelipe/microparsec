import strutils

import results

import primitives
import types

func attempt*[T](parser: Parser[T]): Parser[T] {.inline.} =
  ## Create a `Parser` that attempts a parse, and if it fails, rewind the
  ## input so that no input appears to have been consumed.
  ##
  ## This function is called `try` in Parsec, but this conflicts with the
  ## `try` keyword in Nim.
  ##
  ## This combinator is provided for compatibility with Parsec. We follow
  ## Attoparsec's implementation, which always backtracks on failure.
  ##
  ## **Note**: We mean to deprecate this function once we're past 0.1 or so.
  parser

func `<?>`*[T](parser: Parser[T], expected: string): Parser[T] {.inline.} =
  ## Build a `Parser` that behaves as `parser`, but whenever `parser` fails,
  ## it replaces expect error messages with `expected`. As such, this function
  ## effectively names a parser, in case failure occurs.
  ##
  ## This is normally used at the end of a set alternatives where we want to
  ## return an error message in terms of a higher level construct rather than
  ## returning all possible characters.
  ##
  ## **Note**: In the future, this might become a template, so that functions
  ## such as `satisfy` won't need a `expected` parameter for performance
  ## reasons.
  return proc(state: ParseState): ParseResult[T] =
    let res = parser(state)
    if res.isOk:
      res
    else:
      fail[T](res.error.unexpected, @[expected], state, res.error.message)

func choice*[T](parsers: openArray[Parser[T]]): Parser[T] =
  ## A `Parser` that tries to apply the actions in a list in order, until one
  ## of them succeeds. Returns the value of the succeeding action.
  ##
  ## **Note**: This might use `varargs` in the future, but this proves to be
  ## more convenient. Furthermore, the current behavior is undefined for an
  ## empty list of parsers. In the future, this will be based in an
  ## Alternative `empty`.

  # The following solves "Error: 'parsers' is of type
  # <varargs[Parser[system.char]]> which cannot be captured as it would
  # violate memory safety...". See discussion in
  # <https://github.com/nim-lang/Nim/issues/17187>.
  let parsers = @parsers

  return proc(state: ParseState): ParseResult[T] =
    var expecteds, messages: seq[string]
    for parser in parsers:
      result = parser(state)
      if result.isOk:
        return
      else:
        # Update reported data, so that it matches the state
        if result.error.message notin messages:
          messages.add result.error.message
        for expected in result.error.expected:
          if expected notin expecteds:
            expecteds.add expected
        continue

    result = fail[T](
      result.error.unexpected,
      expecteds,
      state,
      join(messages),
    )

func optional*[T](parser: Parser[T]): Parser[void] {.inline.} =
  ## Create a `Parser` that tries to apply `parser`. It might consume input if
  ## `parser` is successful and consumes input. And due to backtracking, it
  ## never fails. The result of `parser` is discarded.
  parser >> pure() <|> pure()

func between*[R, S, T](open: Parser[R], parser: Parser[T], close: Parser[
    S]): Parser[T] {.inline.} =
  ## Create a `Parser` that parses `open`, followed by `parser` and then
  ## `close`, returning the value given by `parser`.
  (open >> parser).flatMap do (x: T) -> Parser[T]:
    close >> pure x

func many1*[T](parser: Parser[T]): Parser[seq[T]] {.inline.} =
  ## Build a `Parser` that applies another `Parser` *one* or more times and
  ## returns a sequence of the parsed values.
  parser.flatMap do (x: T) -> Parser[seq[T]]:
    many(parser).flatMap do (xs: seq[T]) -> Parser[seq[T]]:
      pure x & xs

func sepBy1*[S, T](parser: Parser[T], separator: Parser[S]): Parser[seq[T]] {.inline.} =
  ## Create a `Parser` that parses a sequence of *one* or more occurrences of
  ## `parser`, separated by `separator`.
  parser.flatMap do (x: T) -> Parser[seq[T]]:
    many(separator >> parser).flatMap do (xs: seq[T]) -> Parser[seq[T]]:
      pure x & xs

func sepBy*[S, T](parser: Parser[T], separator: Parser[S]): Parser[seq[T]] {.inline.} =
  ## Create a `Parser` that parses a sequence of *zero* or more occurrences of
  ## `parser`, separated by `separator`.
  sepBy1(parser, separator) <|> pure[seq[T]](@[])

proc anyToken(state: ParseState): ParseResult[char] {.inline.} =
  ## A `Parser` that accepts any kind of token and returns the accepted token.
  ParseResult[char].ok state.readChar

proc eof*(state: ParseState): ParseResult[void] =
  ## A `Parser` that only succeeds at the end of the input. This is not a
  ## primitive parser but it is defined using `notFollowedBy`.
  if state.atEnd:
    ParseResult[void].ok
  else:
    fail[void](quoted state.peekChar, @["end of input"], state, message = "eof")

func notFollowedBy[T](parser: Parser[T]): Parser[void] {.inline.} =
  ## A `Parser` that only succeeds when `parser` fails. This parser does not
  ## consume any input. This parser can be used to implement the
  ## "longest match" rule.
  return func(state: ParseState): ParseResult[void] =
    let
      position = state.getPosition
      res = parser(state)
    if res.isErr:
      ParseResult[void].ok
    else:
      fail[void](quoted res.get, @[], state, message = "notFollowedBy")
    state.setPosition(position)
