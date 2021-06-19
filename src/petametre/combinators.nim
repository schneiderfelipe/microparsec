import streams
import sugar

import results

import primitives
import types

# TODO: choice
# TODO: option
# TODO: optionMaybe
# TODO: optional

func between*[R,S,T](open: Parser[R], parser: Parser[T], close: Parser[S]): Parser[T] {.inline.} =
  ## Create a `Parser` that parses `open`, followed by `parser` and then
  ## `close`, returning the value given by `parser`.
  discard

# TODO: skipMany1

func many1*[T](parser: Parser[T]): Parser[seq[T]] {.inline.} =
  ## Build a `Parser` that applies another `Parser` *one* or more times and
  ## returns a sequence of the parsed values.
  parser >>= (
    (x: T) => many(parser) >>= (
      (xs: seq[T]) => pure(x & xs)
    )
  )

func sepBy*[S,T](parser: Parser[T], separator: Parser[S]): Parser[seq[T]] {.inline.} =
  ## Create a `Parser` that parses a sequence of *zero* or more occurrences of
  ## `parser`, separated by `separator`.
  discard

func sepBy1*[S,T](parser: Parser[T], separator: Parser[S]): Parser[seq[T]] {.inline.} =
  ## Create a `Parser` that parses a sequence of *one* or more occurrences of
  ## `parser`, separated by `separator`.
  discard

# TODO: sepEndBy1
# TODO: sepEndBy
# TODO: endBy1
# TODO: endBy
# TODO: count
# TODO: chainr
# TODO: chainl
# TODO: chainl1
# TODO: chainr1
# TODO: anyToken

# TODO: define eof in function of ch or something else
# TODO: this function returns a null character if it succeeds. Check if
# that's what Parsec does or not (it might return an empty string instead, do
# what it does there).
proc eof*(s: Stream): ParseResult[char] =
  if s.atEnd:
    ParseResult[char].ok('\x00')
  else:
    ParseResult[char].err(
      (s.getPosition, $s.peekChar, @["end of input"])
    )

# TODO: notFollowedBy
# TODO: manyTill
# TODO: lookAhead
# TODO: parserTrace
# TODO: parserTraced
