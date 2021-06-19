import streams
import sugar

import results

type
  ParseError = tuple
    ## A `ParseError` contains both what was expected by the `Parser`, what
    ## was actually found by it (the unexpected `string`) and the `Stream`
    ## position.
    position: int
    unexpected: string
    expected: seq[string]

  ParseResult*[T] = Result[T,ParseError]
    ## A `ParseResult` of type `T` contains either a parsed object of that
    ## type or a `ParseError`.

  Parser*[T] = Stream -> ParseResult[T]
    ## A `Parser` for a type `T` is a function that receives a `Stream` and
    ## gives back a `ParseResult` of the same type.
