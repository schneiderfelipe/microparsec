import strutils
import sugar

import results

import primitives
import types

template attempt*[T](parser: Parser[T]): Parser[T] =
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
  return func(state: ParseState): ParseResult[T] =
    let res = parser state
    if res.isOk:
      return res
    return fail[T](res.error.unexpected, [expected], state, res.error.message)

func choice*[T](parsers: openArray[Parser[T]]): Parser[T] {.inline.} =
  ## A `Parser` that tries to apply the actions in a sequence in order, until
  ## one of them succeeds. Returns the value of the succeeding action.
  ##
  ## **Note**: This might use `varargs` in the future, but this proves to be
  ## more convenient. Furthermore, the current behavior is undefined for an
  ## empty sequence of parsers. In the future, this will be based in an
  ## Alternative `empty`.

  # The following solves "Error: 'parsers' is of type
  # <varargs[Parser[system.char]]> which cannot be captured as it would
  # violate memory safety...". See discussion in
  # <https://github.com/nim-lang/Nim/issues/17187>.
  let parsers = @parsers

  return func(state: ParseState): ParseResult[T] =
    # We could use OrderedSet[string] in the future
    var expecteds, messages: seq[string]
    for parser in parsers:
      result = parser state
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
      join messages,
    )

template option*[T](x: T, parser: Parser[T]): Parser[T] =
  ## A `Parser` that tries to apply an action. If it fails without consuming
  ## input, it returns the value `x`, otherwise the value returned by the
  ## action.
  parser <|> pure x

template many1*[T](parser: Parser[T]): Parser[seq[T]] =
  ## Build a `Parser` that applies another `Parser` *one* or more times.
  ## Returns a sequence of the parsed values.
  liftA2((x: T, xs: seq[T]) => x & xs, parser, many parser)

template sepBy*[S, T](parser: Parser[T], separator: Parser[S]): Parser[seq[T]] =
  ## A `Parser` that applies *zero* or more occurrences of `parser`, separated
  ## by `separator`. Returns a sequence of the values returned by `parser`.
  ##
  ## **Note**: Attoparsec's implementation seems to be too complicated,
  ## (unnecessarily?) testing for a first occurrence. Maybe there's a
  ## performance reason, I don't know. Ours might be simpler, but there might
  ## be a catch somewhere. In any case, tests pass. Be advised, and report any
  ## issue you might find!
  option(newSeq[T](), sepBy1(parser, separator))

func sepBy1*[S, T](parser: Parser[T], separator: Parser[S]): Parser[seq[T]] {.inline.} =
  ## Create a `Parser` that applies *one* or more occurrences of
  ## `parser`, separated by `separator`. Returns a sequence of the values
  ## returned by `parser`.
  ##
  ## **Note**: Currently, `sepBy` is defined in function of this function.
  ## This might change in the future, if the implementation can be made
  ## simpler some other way.
  return func(state: ParseState): ParseResult[seq[T]] =
    var
      value: ParseResult[T] = parser state
      values: seq[T]
    if value.isOk:
      values.add value.get
      let separatorParser = separator >> parser
      while (value = separatorParser state; value.isOk):
        values.add value.get
      return ParseResult[seq[T]].ok values
    return fail[seq[T]](
      value.error.unexpected,
      value.error.expected,
      state,
      value.error.message,
    )

func manyTill*[S, T](parser: Parser[T], endparser: Parser[S]): Parser[seq[T]] {.inline.} =
  ## A `Parser` that applies an action *zero* or more times until another
  ## action succeeds, and returns the sequence of values returned by the first
  ## action.
  ##
  ## **Note**: Error messages are not good enough yet, but the current
  ## implementation is comparable to Attoparsec's.
  return func(state: ParseState): ParseResult[seq[T]] =
    var
      quit: ParseResult[S]
      value: ParseResult[T]
      values: seq[T]
    while (quit = endparser state; quit.isErr):
      value = parser state
      if value.isOk:
        values.add value.get
      else:
        return fail[seq[T]](
          value.error.unexpected,
          value.error.expected,
          state,
          value.error.message,
        )
    return ParseResult[seq[T]].ok values

func skipMany*[T](parser: Parser[T]): Parser[void] {.inline.} =
  ## Build a `Parser` that skips *zero* or more instances of an action.
  return func(state: ParseState): ParseResult[void] =
    var value: ParseResult[T]
    while (value = parser state; value.isOk):
      discard
    ParseResult[void].ok

template skipMany1*[T](parser: Parser[T]): Parser[void] =
  ## Build a `Parser` that skips *one* or more instances of an action.
  parser >> skipMany parser

func count*[T](n: int, parser: Parser[T]): Parser[seq[T]] {.inline.} =
  ## A `Parser` that applies the given action repeatedly, returning every
  ## result.
  ##
  ## **Note**: This short circuits in case of errors.
  return func(state: ParseState): ParseResult[seq[T]] =
    var
      value: ParseResult[T]
      values: seq[T]
    for _ in 0..<n:
      value = parser state
      if value.isOk:
        values.add value.get
      else:
        # Short circuit
        return fail[seq[T]](
          value.error.unexpected,
          value.error.expected,
          state,
          value.error.message,
        )
    ParseResult[seq[T]].ok values

template optional*[T](parser: Parser[T]): Parser[void] =
  ## Create a `Parser` that tries to apply `parser`. It might consume input if
  ## `parser` is successful and consumes input. And due to backtracking, it
  ## never fails. The result of `parser` is discarded.
  parser >> pure() <|> pure()

template between*[R, S, T](open: Parser[R], parser: Parser[T], close: Parser[
    S]): Parser[T] =
  ## Create a `Parser` that parses `open`, followed by `parser` and then
  ## `close`, returning the value given by `parser`.
  ##
  ## **Note**: This function is experimental.
  (open >> parser).flatMap do (x: T) -> Parser[T]:
    close >> pure x

template anyToken(state: ParseState): ParseResult[char] =
  ## A `Parser` that accepts any kind of token and returns the accepted token.
  ParseResult[char].ok state.readChar

template eof*(state: ParseState): ParseResult[void] =
  ## A `Parser` that only succeeds at the end of the input. This is not a
  ## primitive parser but it is defined using `notFollowedBy`.
  if state.atEnd:
    return ParseResult[void].ok
  return fail[void](quoted state.peekChar, ["end of input"], state,
      message = "eof")

func notFollowedBy[T](parser: Parser[T]): Parser[void] {.inline.} =
  ## A `Parser` that only succeeds when `parser` fails. This parser does not
  ## consume any input. This parser can be used to implement the
  ## "longest match" rule.
  return func(state: ParseState): ParseResult[void] =
    let
      position = state.getPosition
      res = parser state
    result = if res.isErr:
      ParseResult[void].ok
    else:
      fail[void](quoted res.get, [], state, message = "notFollowedBy")
    state.setPosition position
