import sugar

import results

import primitives
import types

func optional*[T](parser: Parser[T]): Parser[void] {.inline.} =
  ## Create a `Parser` that tries to apply `parser`. It might consume input if
  ## `parser` is successful and consumes input. And due to backtracking, it
  ## never fails. The result of `parser` is discarded.
  parser >> pure() <|> pure()

func between*[R,S,T](open: Parser[R], parser: Parser[T], close: Parser[S]): Parser[T] {.inline.} =
  ## Create a `Parser` that parses `open`, followed by `parser` and then
  ## `close`, returning the value given by `parser`.
  open >> parser >>= (
    (x: T) => close >> pure(x)
  )

func many1*[T](parser: Parser[T]): Parser[seq[T]] {.inline.} =
  ## Build a `Parser` that applies another `Parser` *one* or more times and
  ## returns a sequence of the parsed values.
  parser >>= (
    (x: T) => many(parser) >>= (
      (xs: seq[T]) => pure(x & xs)
    )
  )

func sepBy1*[S,T](parser: Parser[T], separator: Parser[S]): Parser[seq[T]] {.inline.} =
  ## Create a `Parser` that parses a sequence of *one* or more occurrences of
  ## `parser`, separated by `separator`.
  parser >>= (
    (x: T) => many(separator >> parser) >>= (
      (xs: seq[T]) => pure(x & xs)
    )
  )

func sepBy*[S,T](parser: Parser[T], separator: Parser[S]): Parser[seq[T]] {.inline.} =
  ## Create a `Parser` that parses a sequence of *zero* or more occurrences of
  ## `parser`, separated by `separator`.
  sepBy1(parser, separator) <|> pure[seq[T]](@[])

proc anyToken(state: ParseState): ParseResult[char] {.inline.} =
  ## A `Parser` that accepts any kind of token and returns the accepted token.
  ParseResult[char].ok(state.readChar)

proc eof*(state: ParseState): ParseResult[void] =
  ## A `Parser` that only succeeds at the end of the input. This is not a
  ## primitive parser but it is defined using `notFollowedBy`.
  if state.atEnd:
    ParseResult[void].ok()
  else:
    failure[void](singleton state.peekChar, @["end of input"], state)

func notFollowedBy[T](parser: Parser[T]): Parser[void] {.inline.} =
  ## A `Parser` that only succeeds when `parser` fails. This parser does not
  ## consume any input. This parser can be used to implement the
  ## "longest match" rule.
  return func(state: ParseState): ParseResult[void] =
    let
      position = state.getPosition
      res = parser(state)
    if res.isErr:
      ParseResult[void].ok()
    else:
      failure[void](singleton res.get, @[], state)
    state.setPosition(position)
