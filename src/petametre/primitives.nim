import sugar

import results

import types

func `>>=`*[S,T](parser0: Parser[S], f: S -> Parser[T]): Parser[T] {.inline.} =
  ## Pass the result of a `Parser` to a function that returns another `Parser`.
  ##
  ## This is required in monadic parsing.
  return proc(state: ParseState): ParseResult[T] =
    let result0 = parser0(state)
    if result0.isOk:
      f(result0.get)(state)
    else:
      failure[T](result0)

# TODO: <|> has different semantics from Parsec's <|> w.r.t. backtracking.
# This might be either good or bad. But the current implementation is
# definitely useful.
# TODO: implement choice as well and ensure a full alternative instance.
func `<|>`*[T](parser0, parser1: Parser[T]): Parser[T] {.inline.} =
  ## Create a `Parser` as a choice combination between two other `Parser`s.
  return proc(state: ParseState): ParseResult[T] =
    let result0 = parser0(state)
    if result0.isOk:
      result0
    else:
      let result1 = parser1(state)
      if result1.isOk:
        result1
      else:
        assert result0.error.unexpected == result1.error.unexpected
        failure[T](result0.error.unexpected, result0.error.expected & result1.error.expected, state)

func pure*(): Parser[void] {.inline.} =
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
    ParseResult[T].ok(x)

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
    ParseResult[seq[T]].ok(elems)

func `<?>`*[T](parser: Parser[T], expected: string): Parser[T] {.inline.} =
  ## Build a `Parser` that behaves as `parser`, but whenever `parser` fails,
  ## it replaces expect error messages with `expected`.
  ##
  ## This is normally used at the end of a set alternatives where we want to
  ## return an error message in terms of a higher level construct rather than
  ## returning all possible characters.
  return proc(state: ParseState): ParseResult[T] =
    let res = parser(state)
    if res.isOk:
      res
    else:
      failure[T](res.error.unexpected, @[expected], state)

func `>>`*[S,T](parser0: Parser[S], parser1: Parser[T]): Parser[T] {.inline.} =
  parser0 >>= ((_: S) => parser1)
