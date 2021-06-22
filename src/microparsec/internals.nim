import sugar

import results

import primitives
import types

func satisfy*(predicate: char -> bool, expected: seq[string] = @[]): Parser[
    char] {.inline.} =
  ## Create a `Parser` that succeeds for any character for which a predicate
  ## returns `true`. Returns the character that is actually parsed.
  ##
  ## This is used to build more complex `Parser`s.
  return proc(state: ParseState): ParseResult[char] =
    if not state.atEnd:
      let h = state.readChar
      if predicate(h):
        ParseResult[char].ok(h)
      else:
        state.stepBack
        fail[char](quoted h, expected, state, message = "satisfy")
    else:
      fail[char]("end of input", expected, state, message = "satisfy")

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
