import streams
import sugar

import results

type
  ParseError = tuple
    ## A `ParseError` contains both what was expected by the `Parser`, and
    ## what was actually found by it (the unexpected `string`).
    unexpected: string
    expected: seq[string]

  ParseState* = int  # tuple
    ## A `ParseState` keeps track of the position we are in the `Stream`.
    # position: int

  ParseResult*[T] = Result[T,ParseError]  # tuple
    ## A `ParseResult` of type `T` contains both a `Result` object (with
    ## either a parsed object of type `T` or a `ParseError`) and a
    ## `ParseState`.
    # result:
    # state: ParseState

  Parser*[T] = Stream -> (ParseResult[T], ParseState)
    ## A `Parser` for a type `T` is a function that receives a `Stream` and
    ## gives back a `ParseResult` of the same type.
