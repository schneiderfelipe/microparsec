import streams

import results

import primitives
import types

# TODO: choice
# TODO: option
# TODO: optionMaybe
# TODO: optional

func between*[R,S,T](open: Parser[R], parser: Parser[T], close: Parser[S]): Parser[T] =
  ## Create a `Parser` that parses `open`, followed by `parser` and then
  ## `close`, returning the value given by `parser`.
  discard

# TODO: skipMany1

# TODO: we might specialize this for char and string in the future, as
# Haskell considers strings as sequences of characters. But this might not be
# necessary (except if for performance, I'm not sure), because the current
# implementation works out of the box already! (Which is amazing...)
# TODO: check error messages from Parsec and duplicate them here.
func many1*[T](parser: Parser[T]): Parser[seq[T]] {.inline.} =
  ## Build a `Parser` that applies another `Parser` one or more times.
  parser >>= proc(x: T): Parser[seq[T]] =
    (many1(parser) <|> pure(newSeq[T]())) >>= proc(xs: seq[T]): Parser[seq[T]] =
      pure(x & xs)

func sepBy*[S,T](parser: Parser[T], separator: Parser[S]): Parser[seq[T]] =
  ## Create a `Parser` that parses a sequence of *zero* or more occurrences of
  ## `parser`, separated by `separator`.
  discard

func sepBy1*[S,T](parser: Parser[T], separator: Parser[S]): Parser[seq[T]] =
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
