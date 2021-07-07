## Commonly used character parsers.

import strutils
import sugar

import combinators
import internals
import types

func inClass*(cs: auto): (char -> bool) =
  ## Match any character in a set.
  return proc(c: char): bool =
    c in cs

func notInClass*(cs: auto): (char -> bool) =
  ## Match any character not in a set.
  return proc(c: char): bool =
    c notin cs

func oneOf*(cs: auto): Parser[char] =
  ## A `Parser` that succeeds if the current character is in the supplied set
  ## of characters. Returns the parsed character. See also ``satisfy``.
  runnableExamples:
    let vowel = oneOf "aeiou"
  satisfy inClass(cs)

func noneOf*(cs: auto): Parser[char] =
  ## As the dual of ``oneOf``, this `Parser` succeeds if the current character
  ## is *not* in the supplied set of characters. Returns the parsed character.
  runnableExamples:
    let consonant = noneOf "aeiou"
  satisfy notInClass(cs)

let
  space*: Parser[char] =
    satisfy(isSpaceAscii, ["space"])
    ## A `Parser` that consumes a single white space character (any character
    ## which satisfies `isSpaceAscii`). Returns the parsed character.

  spaces*: Parser[void] =
    skipMany space <?> "space"
    ## Skips *zero* or more white space characters. See also ``skipMany``.
