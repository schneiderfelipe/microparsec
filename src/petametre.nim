# TODO: we need notation for doing stuff like the following:
#
# xml = do{ name <- openTag
#         ; content <- many xml
#         ; endTagname
#         ; pure (Node name content)
#         } <|> xmlText

import strutils
export toUpperAscii, toHex

import sugar
export `=>`, `->`

import streams
export newStringStream

import options
export Option, some, none

import results
export ok, err, `==`

import petametre/combinators
import petametre/primitives
import petametre/types
export many, many1, `<|>`, `pure`, `eof`, `>>=`

func identity*[T](x: T): T =
  ## Identity function.
  x

func compose*[R,S,T](f: R -> S, g: S -> T): R -> T =
  ## Compose two functions.
  (x: R) => g(f(x))

func `<?>`*[T](parser: Parser[T], expected: string): Parser[T] {.inline.} =
  return func(s: Stream): ParseResult[T] =
    let res = parser(s)
    if res.isOk:
      res
    else:
      ParseResult[T].err(
        (res.error.position, res.error.unexpected, @[expected])
      )

# `satisfy` could be defined in terms of anyChar, but I find the following
# implementation simpler.
func satisfy(predicate: char -> bool, expected: seq[string] = @[]): Parser[char] {.inline.} =
  ## Create a `Parser` that consumes a single character if it satisfies a
  ## given predicate.
  ##
  ## This is used to build more complex `Parser`s.
  return proc(s: Stream): ParseResult[char] =
    if s.atEnd:
      ParseResult[char].err(
        (s.getPosition, "end of input", expected)
      )
    else:
      let c = s.readChar
      if predicate(c):
        ParseResult[char].ok(c)
      else:
        s.setPosition(s.getPosition - 1)
        ParseResult[char].err(
          (s.getPosition, $c, expected)
        )

# TODO: by inverting the order of the parameters, we can use Nim do-blocks
# for defining mapping functions.
func fmap*[S,T](f: S -> T, parser: Parser[S]): Parser[T] {.inline.} =
  ## Apply a function to the result of a `Parser`.
  ##
  ## This is required in "functor" parsing.
  return proc(s: Stream): ParseResult[T] =
    let result0 = parser(s)
    if result0.isOk:
      ParseResult[T].ok(f(result0.get))
    else:
      ParseResult[T].err(result0.error)

# TODO: the parameter order might be swapped here. Take a look at arrow-style
# combinators.
func `<*>`*[S,T](parser0: Parser[S -> T], parser1: Parser[S]): Parser[T] {.inline.} =
  ## Apply the function parsed by a `Parser` to the result of another
  ## `Parser`.
  ##
  ## This is required in applicative parsing.
  return func(s: Stream): ParseResult[T] =
    let result0 = parser0(s)
    if result0.isOk:
      fmap(result0.get, parser1)(s)
    else:
      ParseResult[T].err(result0.error)

# TODO: maybe we should wrap characters in error messages in single quotes.
func ch*(c: char): Parser[char] {.inline.} =
  ## Create a `Parser` that consumes a specific single character if present.
  ##
  ## This function is called `char` in Parsec, but this conflicts with the
  ## type `char` in Nim.
  satisfy((d: char) => d == c, @[$c])

let letter*: Parser[char] =
  satisfy(isAlphaAscii, @["letter"])
  ## A `Parser` that consumes any letter.

let digit*: Parser[char] =
  satisfy(isDigit, @["digit"])
  ## A `Parser` that consumes any digit.

func `>>`*[S,T](parser0: Parser[S], parser1: Parser[T]): Parser[T] {.inline.} =
  parser0 >>= ((_: S) => parser1)

# TODO: this currently always returns an empty string if successful, which is
# not good!
# TODO: furthermore, I would like to avoid recursiveness here as well.
func str*(s: string): Parser[string] {.inline.} =
  ## Build a `Parser` that consumes a given string if present.
  ##
  ## This function is called `string` in Parsec, but this conflicts with the
  ## type `string` in Nim.
  (if s == "":
    pure(s)
  else:
    ch(s[0]) >> str(s[1..^1])
  ) <?> s

# TODO: this is apparently not part of the standard Parsec
let identifier*: Parser[seq[char]] =
  many1(letter <|> digit <|> ch('_'))
  ## A `Parser` that consumes a common identifier, made of letters, digits
  ## and underscores (`'_'`).

# TODO: attempt has different semantics from Parsec's try. This might be
# either good or bad. But the current implementation is definitely useful.
func attempt*[T](parser: Parser[T]): Parser[Option[T]] {.inline.} =
  ## Create a `Parser` that behaves exactly like the given one, but never
  ## fails. The failure state is modeled as an `Option` of type `T`.
  ##
  ## This function is called `try` in Parsec, but this conflicts with the
  ## `try` keyword in Nim.
  return func(s: Stream): ParseResult[Option[T]] =
    let res = parser(s)
    if res.isOk:
      ParseResult[Option[T]].ok(some(res.get))
    else:
      ParseResult[Option[T]].ok(none(T))


# Also known as `item`.
# TODO: an old definition explicitly checked for end of input. This is
# now done in satisfy. Check Parsec's current implementation.
let anyChar*: Parser[char] =
  satisfy((_: char) => true, @["any character"])

# TODO: implement `many`, `between` and the probably others suggested in
# <http://theorangeduck.com/page/you-could-have-invented-parser-combinators>.

func `<$`*[S,T](x: T, parser: Parser[S]): Parser[T] =
  fmap((_: S) => x, parser)

func `*>`*[S,T](parser0: Parser[S], parser1: Parser[T]): Parser[T] =
  return func(s: Stream): ParseResult[T] =
    discard parser0(s)
    parser1(s)

func `<*`*[T,S](parser0: Parser[T], parser1: Parser[S]): Parser[T] =
  return func(s: Stream): ParseResult[T] =
    result = parser0(s)
    discard parser1(s)

func parse*[T](parser: Parser[T], s: Stream): ParseResult[T] =
  parser s

func parse*[T](parser: Parser[T], s: string): ParseResult[T] =
  parser newStringStream(s)
