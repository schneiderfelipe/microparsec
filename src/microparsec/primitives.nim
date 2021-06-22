import sugar

import results

import types

template quoted*(x: auto): string =
  ## Quote object `x` and return as string.
  var q: string
  addQuoted(q, x)
  q

func flatMap*[S, T](parser: Parser[S], f: S -> Parser[T]): Parser[T] {.inline.} =
  ## Pass the result of a `Parser` to a function that returns another `Parser`.
  ##
  ## This is the equivalent to `bind` or `>>=` in Haskell, and is required in
  ## monadic parsing.
  return proc(state: ParseState): ParseResult[T] =
    let res = parser(state)
    if res.isOk:
      f(res.get)(state)
    else:
      fail[T](res)

func `<|>`*[T](parser0, parser1: Parser[T]): Parser[T] {.inline.} =
  ## Create a `Parser` as a choice combination between two other `Parser`s.
  return proc(state: ParseState): ParseResult[T] =
    let res0 = parser0(state)
    if res0.isOk:
      res0
    else:
      let res1 = parser1(state)
      if res1.isOk:
        res1
      else:
        # Report the last found piece, so that it matches the state
        let message = if res0.error.message != res1.error.message:
          res0.error.message & res1.error.message
        else:
          res0.error.message
        fail[T](
          res1.error.unexpected,
          res0.error.expected & res1.error.expected,
          state,
          message,
        )

func pure*: Parser[void] {.inline.} =
  ## Create a `Parser` that returns nothing and consumes nothing. As such,
  ## it never fails.
  ##
  ## This is required in both applicative and monadic parsers.
  return proc(state: ParseState): ParseResult[void] =
    ParseResult[void].ok

func pure*[T](x: T): Parser[T] {.inline.} =
  ## Create a `Parser` that always return `x`, but consumes nothing. As such,
  ## it never fails.
  ##
  ## This is required in both applicative and monadic parsers.
  return proc(state: ParseState): ParseResult[T] =
    ParseResult[T].ok x

func many*[T](parser: Parser[T]): Parser[seq[T]] {.inline.} =
  ## Build a `Parser` that applies another `Parser` *zero* or more times and
  ## returns a sequence of the parsed values.
  return func(state: ParseState): ParseResult[seq[T]] =
    var
      elems: seq[T]
      res = parser(state)
    if res.isOk:
      elems.add(res.get)
      while (res = parser(state); res.isOk):
        elems.add(res.get)
    ParseResult[seq[T]].ok elems

func `<?>`*[T](parser: Parser[T], expected: string): Parser[T] {.inline.} =
  ## Build a `Parser` that behaves as `parser`, but whenever `parser` fails,
  ## it replaces expect error messages with `expected`.
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

func `>>`*[S, T](parser0: Parser[S], parser1: Parser[T]): Parser[T] {.inline.} =
  parser0.flatMap do (_: S) -> Parser[T]:
    parser1
