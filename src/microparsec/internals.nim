import sugar

import results

import primitives
import types

# `satisfy` could be defined in terms of anyChar, but I find the following
# implementation simpler.
func satisfy*(predicate: char -> bool, expected: seq[string] = @[]): Parser[
    char] {.inline.} =
  ## Create a `Parser` that consumes a single character if it satisfies a
  ## given predicate.
  ##
  ## This is used to build more complex `Parser`s.
  return proc(state: ParseState): ParseResult[char] =
    if state.atEnd:
      failure[char]("end of input", expected, state)
    else:
      let c = state.readChar
      if predicate(c):
        ParseResult[char].ok(c)
      else:
        state.stepBack
        failure[char](quoted c, expected, state)

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
