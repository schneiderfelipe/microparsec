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
      if p h:
        ParseResult[char].ok h
      else:
        state.stepBack
        fail[char](quoted h, expected, state, message = "satisfy")
    else:
      fail[char]("end of input", expected, state, message = "satisfy")

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
      if p h:
        ParseResult[void].ok
      else:
        state.stepBack
        fail[void](quoted h, expected, state, message = "satisfy")
    else:
      fail[void]("end of input", expected, state, message = "satisfy")

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
      if p c:
        ParseResult[T].ok c
      else:
        state.stepBack
        fail[T](quoted c, expected, state, message = "satisfyWith")
    else:
      fail[T]("end of input", expected, state, message = "satisfyWith")

func inClass*[T](s: T): (char -> bool) =
  ## Match any character in a set.
  return proc(c: char): bool =
    c in s

func notInClass*[T](s: T): (char -> bool) =
  ## Match any character not in a set.
  return proc(c: char): bool =
    c notin s

let anyChar*: Parser[char] =
  satisfy((_: char) => true, ["any character"])
  ## A `Parser` that matches any character.

func ch*(c: char): Parser[char] {.inline.} =
  ## Create a `Parser` that matches a specific character.
  ##
  ## This function is called `char` in Parsec, but this conflicts with the
  ## type `char` in Nim.
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
    ParseResult[Option[char]].ok some(state.peekChar)
  else:
    ParseResult[Option[char]].ok none(char)

proc peekChF*(state: ParseState): ParseResult[char] =
  ## A `Parser` that matches any character, to perform lookahead. Does not
  ## consume any input, but will fail if end of input has been reached.
  ##
  ## This is currently called `peekCh` to avoid conflict with other function,
  ## but that will change in the future.
  if not state.atEnd:
    ParseResult[char].ok state.peekChar
  else:
    fail[char]("end of input", ["any character"], state, message = "peekChF")
