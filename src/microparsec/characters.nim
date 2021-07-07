## Commonly used character parsers.

import strutils
import sugar

import types
import internals

func inClass*(cs: auto): (char -> bool) =
  ## Match any character in a set.
  return proc(c: char): bool =
    c in cs

func oneOf*(cs: auto): Parser[char] =
  ## A `Parser` that succeeds if the current character is in the supplied set
  ## of characters. Returns the parsed character. See also ``satisfy``.
  runnableExamples:
    let vowel = oneOf "aeiou"
  satisfy inClass(cs)

let
  space*: Parser[char] =
    satisfy(isSpaceAscii, ["space"])
    ## A `Parser` that consumes a single space character, as recognised by
    ## `isSpaceAscii`.
