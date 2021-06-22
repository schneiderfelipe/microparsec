import sugar

import results

import primitives
import types

func satisfy*(p: char -> bool, expected: seq[string] = @[]): Parser[char] {.inline.} =
  ## Create a `Parser` that succeeds for any character for which a predicate
  ## returns `true`. Returns the character that is actually parsed.
  ##
  ## This can be used to build more complex `Parser`s.
  return proc(state: ParseState): ParseResult[char] =
    if not state.atEnd:
      let h = state.readChar
      if p h:
        ParseResult[char].ok(h)
      else:
        state.stepBack
        fail[char](quoted h, expected, state, message = "satisfy")
    else:
      fail[char]("end of input", expected, state, message = "satisfy")

func skip*(p: char -> bool, expected: seq[string] = @[]): Parser[void] {.inline.} =
  ## Create a `Parser` that succeeds for any character for which a predicate
  ## returns `true`.
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

func satisfyWith*[T](f: char -> T, p: T -> bool, expected: seq[string] = @[]): Parser[T] {.inline.} =
  ## Create a `Parser` that transforms a character, and succeeds if a
  ## predicate returns `true` on the transformed value. The parser returns the
  ## transformed character that was parsed.
  return proc(state: ParseState): ParseResult[T] =
    if not state.atEnd:
      let c = f state.readChar
      if p c:
        ParseResult[T].ok(c)
      else:
        state.stepBack
        fail[T](quoted c, expected, state, message = "satisfyWith")
    else:
      fail[T]("end of input", expected, state, message = "satisfyWith")

let anyChar*: Parser[char] =
  satisfy((_: char) => true, @["any character"])
  ## A `Parser` that matches any character.

func ch*(c: char): Parser[char] {.inline.} =
  ## Create a `Parser` that matches a specific character.
  ##
  ## This function is called `char` in Parsec, but this conflicts with the
  ## type `char` in Nim.
  satisfy((d: char) => d == c, @[quoted c])

func notChar*(c: char): Parser[char] {.inline.} =
  ## Create a `Parser` that matches any character except the given one.
  satisfy((d: char) => d != c, @["not " & quoted c])
