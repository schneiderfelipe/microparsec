import strutils except spaces
export toUpperAscii, toHex

import sugar
export `=>`, `->`

import options
export Option, some, none

import results
export ok, err, isOk, isErr, `==`

import microparsec/characters
import microparsec/combinators
import microparsec/internals
import microparsec/primitives
import microparsec/types
export Parser, ParseResult, identity, compose, optional,
  anyChar, between, ch, satisfy, skip, satisfyWith, peekCh, peekChF, sepBy,
  sepBy1, many, many1, notChar, `<|>`, `<$`, `<*`, `*>`, liftA2, pure, eof,
  flatMap, `>>`, `$`, debugParse, parse, atEnd, setPosition, getPosition,
  attempt, `<?>`, choice, option, manyTill, skipMany, skipMany1, count, match,
  inClass, notInClass, oneOf, noneOf, space, spaces, newline, crlf, endOfLine

func map*[S, T](parser: Parser[S], f: S -> T): Parser[T] {.inline.} =
  ## Apply a function to the result of a `Parser`.
  ##
  ## This is required in "functor" parsing.
  return proc(state: ParseState): ParseResult[T] =
    let res = parser state
    if res.isOk:
      return ParseResult[T].ok f res.get
    return fail[T]res

func `<*>`*[S, T](parser0: Parser[S -> T], parser1: Parser[S]): Parser[T] {.inline.} =
  ## Apply the function parsed by a `Parser` to the result of another
  ## `Parser`.
  ##
  ## This is required in applicative parsing.
  return func(state: ParseState): ParseResult[T] =
    let res0 = parser0 state
    if res0.isOk:
      return parser1.map(res0.get)state
    return fail[T]res0

func str*(s: string, t = ""): Parser[string] {.inline.} =
  ## Build a `Parser` that consumes a given string if present.
  ##
  ## This function is called `string` in Parsec, but this conflicts with the
  ## type `string` in Nim.
  (if s == "":
    pure t
  else:
    ch(s[0]).flatMap do (c: char) -> Parser[string]:
      str(s[1..^1], t & c)
  ) <?> quoted s

let
  digit*: Parser[char] =
    satisfy(isDigit, ["digit"])
    ## A `Parser` that consumes a single digit, as recognised by `isDigit`.

  letter*: Parser[char] =
    satisfy(isAlphaAscii, ["letter"])
    ## A `Parser` that consumes a single letter, as recognised by
    ## `isAlphaAscii`.
