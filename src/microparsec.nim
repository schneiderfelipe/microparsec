import strutils
export toUpperAscii, toHex

import sugar
export `=>`, `->`

import options
export Option, some, none

import results
export ok, err, isOk, isErr, `==`

import microparsec/combinators
import microparsec/primitives
import microparsec/types
export Parser, ParseResult, optional, between, sepBy, sepBy1, many, many1, `<|>`, `pure`, `eof`, `>>=`, `>>`, `<?>`, `$`, debugParse, parse

func identity*[T](x: T): T =
  ## Identity function.
  x

func compose*[R,S,T](f: R -> S, g: S -> T): R -> T {.inline.} =
  ## Compose two functions.
  (x: R) => g(f(x))

# `satisfy` could be defined in terms of anyChar, but I find the following
# implementation simpler.
func satisfy(predicate: char -> bool, expected: seq[string] = @[]): Parser[char] {.inline.} =
  ## Create a `Parser` that consumes a single character if it satisfies a
  ## given predicate.
  ##
  ## This is used to build more complex `Parser`s.
  return proc(state: ParseState): ParseResult[char] =
    if state.atEnd:
      failure[char]("end of input", expected, state)
    else:
      let c = state.readChar
      if predicate(c):
        ParseResult[char].ok(c)
      else:
        state.stepBack
        failure[char](singleton c, expected, state)

func fmap*[S,T](f: S -> T, parser: Parser[S]): Parser[T] {.inline.} =
  ## Apply a function to the result of a `Parser`.
  ##
  ## This is required in "functor" parsing.
  return proc(state: ParseState): ParseResult[T] =
    let res = parser(state)
    if res.isOk:
      ParseResult[T].ok(f(res.get))
    else:
      failure[T](res)

func `<*>`*[S,T](parser0: Parser[S -> T], parser1: Parser[S]): Parser[T] {.inline.} =
  ## Apply the function parsed by a `Parser` to the result of another
  ## `Parser`.
  ##
  ## This is required in applicative parsing.
  return proc(state: ParseState): ParseResult[T] =
    let res0 = parser0(state)
    if res0.isOk:
      fmap(res0.get, parser1)(state)
    else:
      failure[T](res0)

func ch*(c: char): Parser[char] {.inline.} =
  ## Create a `Parser` that consumes a specific single character if present.
  ##
  ## This function is called `char` in Parsec, but this conflicts with the
  ## type `char` in Nim.
  satisfy((d: char) => d == c, @[singleton c])

func str*(s: string): Parser[string] {.inline.} =
  ## Build a `Parser` that consumes a given string if present.
  ##
  ## This function is called `string` in Parsec, but this conflicts with the
  ## type `string` in Nim.
  (if s == "":
    pure(s)
  else:
    ch(s[0]) >> str(s[1..^1])
  ) <?> singleton s

func attempt*[T](parser: Parser[T]): Parser[Option[T]] {.inline.} =
  ## Create a `Parser` that behaves exactly like the given one, but never
  ## fails. The failure state is modeled as an `Option` of type `T`.
  ##
  ## This function is called `try` in Parsec, but this conflicts with the
  ## `try` keyword in Nim.
  return proc(state: ParseState): ParseResult[Option[T]] =
    let res = parser(state)
    if res.isOk:
      ParseResult[Option[T]].ok(some(res.get))
    else:
      ParseResult[Option[T]].ok(none(T))

func `<$`*[S,T](x: T, parser: Parser[S]): Parser[T] {.inline.} =
  fmap((_: S) => x, parser)

func `*>`*[S,T](parser0: Parser[S], parser1: Parser[T]): Parser[T] {.inline.} =
  return func(state: ParseState): ParseResult[T] =
    discard parser0(state)
    parser1(state)

func `<*`*[T,S](parser0: Parser[T], parser1: Parser[S]): Parser[T] {.inline.} =
  return func(state: ParseState): ParseResult[T] =
    result = parser0(state)
    if result.isOk:
      discard parser1(state)


let
  letter*: Parser[char] =
    satisfy(isAlphaAscii, @["letter"])
    ## A `Parser` that consumes any letter.

  digit*: Parser[char] =
    satisfy(isDigit, @["digit"])
    ## A `Parser` that consumes any digit.

  identifier*: Parser[seq[char]] =
    many1(letter <|> digit <|> ch('_'))
    ## A `Parser` that consumes a common identifier, made of letters, digits
    ## and underscores (`'_'`).

  anyChar*: Parser[char] =
    satisfy((_: char) => true, @["any character"])
