import strutils
export toUpperAscii, toHex

import sugar
export `=>`, `->`

import options
export Option, some, none

import results
export ok, err, isOk, isErr, `==`

import microparsec/combinators
import microparsec/internals
import microparsec/primitives
import microparsec/types
export Parser, ParseResult, optional, anyChar, between, ch, sepBy, sepBy1, many, many1,
    `<|>`, `pure`, `eof`, flatMap, `>>`, `<?>`, `$`, debugParse, parse

func identity*[T](x: T): T =
  ## Identity function.
  x

func compose*[R, S, T](f: R -> S, g: S -> T): R -> T {.inline.} =
  ## Compose two functions.
  (x: R) => g(f(x))

func map*[S, T](parser: Parser[S], f: S -> T): Parser[T] {.inline.} =
  ## Apply a function to the result of a `Parser`.
  ##
  ## This is required in "functor" parsing.
  return proc(state: ParseState): ParseResult[T] =
    let res = parser(state)
    if res.isOk:
      ParseResult[T].ok(f(res.get))
    else:
      failure[T](res)

func `<*>`*[S, T](parser0: Parser[S -> T], parser1: Parser[S]): Parser[T] {.inline.} =
  ## Apply the function parsed by a `Parser` to the result of another
  ## `Parser`.
  ##
  ## This is required in applicative parsing.
  return proc(state: ParseState): ParseResult[T] =
    let res0 = parser0(state)
    if res0.isOk:
      parser1.map(res0.get)(state)
    else:
      failure[T](res0)

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

func `<$`*[S, T](x: T, parser: Parser[S]): Parser[T] {.inline.} =
  parser.map do (_: S) -> T:
    x

func `*>`*[S, T](parser0: Parser[S], parser1: Parser[T]): Parser[T] {.inline.} =
  return func(state: ParseState): ParseResult[T] =
    discard parser0(state)
    parser1(state)

func `<*`*[T, S](parser0: Parser[T], parser1: Parser[S]): Parser[T] {.inline.} =
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
