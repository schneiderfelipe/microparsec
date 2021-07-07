import options
import sugar

import results

import primitives
import types

func satisfy*(p: char -> bool, expected: openArray[string] = []): Parser[
    char] {.inline.} =
  ## Create a `Parser` that succeeds for any character for which a predicate
  ## returns `true`. Returns the character that is actually parsed.
  ##
  ## This can be used to build more complex `Parser`s.

  # The following solves "Error: 'parsers' is of type
  # <varargs[Parser[system.char]]> which cannot be captured as it would
  # violate memory safety...". See discussion in
  # <https://github.com/nim-lang/Nim/issues/17187>.
  let expected = @expected

  return proc(state: ParseState): ParseResult[char] =
    if not state.atEnd:
      let h = state.readChar
      return if p h:
        ParseResult[char].ok h
      else:
        state.stepBack
        fail[char](quoted h, expected, state, message = "satisfy")
    return fail[char]("end of input", expected, state, message = "satisfy")

func skip*(p: char -> bool, expected: openArray[string] = []): Parser[void] {.inline.} =
  ## Create a `Parser` that succeeds for any character for which a predicate
  ## returns `true`.

  # The following solves "Error: 'parsers' is of type
  # <varargs[Parser[system.char]]> which cannot be captured as it would
  # violate memory safety...". See discussion in
  # <https://github.com/nim-lang/Nim/issues/17187>.
  let expected = @expected

  return proc(state: ParseState): ParseResult[void] =
    if not state.atEnd:
      let h = state.readChar
      return if p h:
        ParseResult[void].ok
      else:
        state.stepBack
        fail[void](quoted h, expected, state, message = "satisfy")
    return fail[void]("end of input", expected, state, message = "satisfy")

func satisfyWith*[T](f: char -> T, p: T -> bool, expected: openArray[string] = [
    ]): Parser[T] {.inline.} =
  ## Create a `Parser` that transforms a character, and succeeds if a
  ## predicate returns `true` on the transformed value. The parser returns the
  ## transformed character that was parsed.

  # The following solves "Error: 'parsers' is of type
  # <varargs[Parser[system.char]]> which cannot be captured as it would
  # violate memory safety...". See discussion in
  # <https://github.com/nim-lang/Nim/issues/17187>.
  let expected = @expected

  return proc(state: ParseState): ParseResult[T] =
    if not state.atEnd:
      let c = f state.readChar
      return if p c:
        ParseResult[T].ok c
      else:
        state.stepBack
        fail[T](quoted c, expected, state, message = "satisfyWith")
    return fail[T]("end of input", expected, state, message = "satisfyWith")

let anyChar*: Parser[char] =
  satisfy(constant[char, bool]true, ["any character"])
  ## A `Parser` that matches any character.

func ch*(c: char): Parser[char] {.inline.} =
  ## Create a `Parser` that matches a specific character.
  ##
  ## **Note**: This function is called `char` in Parsec, but this conflicts
  ## with the type `char` in Nim. As such, its name might change any time
  ## soon if I find a better one.
  satisfy((d: char) => d == c, [quoted c])

func notChar*(c: char): Parser[char] {.inline.} =
  ## Create a `Parser` that matches any character except the given one.
  satisfy((d: char) => d != c, ["not " & quoted c])

proc peekCh*(state: ParseState): ParseResult[Option[char]] =
  ## A `Parser` that matches any character, to perform lookahead. Returns
  ## `None` if end of input has been reached. Does not consume any input.
  ##
  ## This is currently called `peekCh` to avoid conflict with other function,
  ## but that will change in the future.
  ##
  ## **Note**: Because this parser does not fail, do not use it with
  ## combinators such as `many`, because such parsers loop until a failure
  ## occurs. Careless use will thus result in an infinite loop.
  if not state.atEnd:
    return ParseResult[Option[char]].ok some state.peekChar
  return ParseResult[Option[char]].ok none char

proc peekChF*(state: ParseState): ParseResult[char] =
  ## A `Parser` that matches any character, to perform lookahead. Does not
  ## consume any input, but will fail if end of input has been reached.
  ##
  ## This is currently called `peekCh` to avoid conflict with other function,
  ## but that will change in the future.
  if not state.atEnd:
    return ParseResult[char].ok state.peekChar
  return fail[char]("end of input", ["any character"], state,
      message = "peekChF")

func match*[T](parser: Parser[T]): Parser[(string, T)] {.inline.} =
  ## A `Parser` that returns both the result of a parse and the portion of the
  ## input that was consumed while it was being parsed.
  return proc(state: ParseState): ParseResult[(string, T)] =
    let
      start = state.getPosition
      res = parser state
      length = state.getPosition - start
    if res.isOk:
      ParseResult[(string, T)].ok (state.readLastStr length, res.get)
    else:
      fail[(string, T)](res.error.unexpected, res.error.expected, state,
          res.error.message)
