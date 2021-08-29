import sugar

import results

import types

func identity*[T](x: T): T =
  ## Identity function.
  x

func compose*[R, S, T](f: R -> S, g: S -> T): R -> T {.inline.} =
  ## Compose two functions.
  (x: R) => g f x

func constant*[S, T](x: T): (S -> T) {.inline.} =
  ## Constant function.
  return func(_: S): T =
    x

template quoted*(x: auto): string =
  ## Quote object `x` and return as string.
  var q: string
  addQuoted(q, x)
  q

func pure*: Parser[void] {.inline.} =
  ## Create a `Parser` that returns nothing and consumes nothing. As such,
  ## it never fails.
  ##
  ## This is required in both applicative and monadic parsers.
  return func(state: ParseState): ParseResult[void] =
    ParseResult[void].ok

func pure*[T](x: T): Parser[T] {.inline.} =
  ## Create a `Parser` that always return `x`, but consumes nothing. As such,
  ## it never fails.
  ##
  ## This is required in both applicative and monadic parsers.
  return func(state: ParseState): ParseResult[T] =
    ParseResult[T].ok x

func liftA2*[R, S, T](f: (R, S) -> T, parser0: Parser[R], parser1: Parser[
    S]): Parser[T] {.inline.} =
  ## Lift a binary function to actions.
  parser0.flatMap do (x: R) -> Parser[T]:
    parser1.flatMap do (y: S) -> Parser[T]:
      pure f(x, y)

func flatMap*[S, T](parser: Parser[S], f: S -> Parser[T]): Parser[T] {.inline.} =
  ## Pass the result of a `Parser` to a function that returns another `Parser`.
  ##
  ## This is the equivalent to `bind` or `>>=` in Haskell, and is required in
  ## monadic parsing.
  return proc(state: ParseState): ParseResult[T] =
    let res = parser state
    if res.isOk:
      return f(res.get)state
    return fail[T]res

func `<|>`*[T](parser0, parser1: Parser[T]): Parser[T] {.inline.} =
  ## Create a `Parser` as a choice combination between two other `Parser`s.
  return func(state: ParseState): ParseResult[T] =
    let res0 = parser0 state
    if res0.isOk:
      return res0

    let res1 = parser1 state
    if res1.isOk:
      return res1

    # Report the last found piece, so that it matches the state
    let message = if res0.error.message != res1.error.message:
      res0.error.message & res1.error.message
    else:
      res0.error.message
    return fail[T](
      res1.error.unexpected,
      res0.error.expected & res1.error.expected,
      state,
      message,
    )

func many*[T](parser: Parser[T]): Parser[seq[T]] {.inline.} =
  ## Build a `Parser` that applies another `Parser` *zero* or more times and
  ## returns a sequence of the parsed values.
  ##
  ## **Note**: This function is experimental.
  return func(state: ParseState): ParseResult[seq[T]] =
    var
      value: ParseResult[T]
      values: seq[T]
    while (value = parser state; value.isOk):
      values.add value.get
    ParseResult[seq[T]].ok values

func `<$`*[S, T](x: T, parser: Parser[S]): Parser[T] {.inline.} =
  parser.map constant[S, T]x

func `<*`*[T, S](parser0: Parser[T], parser1: Parser[S]): Parser[T] {.inline.} =
  return proc(state: ParseState): ParseResult[T] =
             let r0: ParseResult[T] = parser0 state
             if r0.isOk:
               let r1: ParseResult[S] = parser1 state
               return if r1.isOk: r0
                      else: fail[T](r1)
             else:
               return r0

func `*>`*[S, T](parser0: Parser[S], parser1: Parser[T]): Parser[T] {.inline.} =
  return func(state: ParseState): ParseResult[T] =
    let res = parser0 state
    if res.isOk:
      return parser1 state
    return fail[T](
      res.error.unexpected,
      res.error.expected,
      state,
      res.error.message,
    )

template `>>`*[S, T](parser0: Parser[S], parser1: Parser[T]): Parser[T] =
  parser0.flatMap constant[S, Parser[T]]parser1
