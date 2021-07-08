## Commonly used character parsers.

import strutils
import sugar

import combinators
import internals
import primitives
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
    skipMany(space) <?> "white space"
    ## Skips *zero* or more white space characters. See also ``skipMany``.

  newline*: Parser[char] =
    ch('\n') <?> "lf new-line"
    ## Parses a newline character (`'\n'`). Returns a newline character.

  crlf*: Parser[char] =
    ch('\r') *> ch('\n') <?> "crlf new-line"
    ## Parses a carriage return character (`'\r'`) followed by a newline
    ## character (`'\n'`). Returns a newline character.

  endOfLine*: Parser[char] =
    newline <|> crlf <?> "new-line"
    ## Parses a CRLF (see ``crlf``) or LF (see ``newline``) end-of-line.
    ## Returns a newline character (`'\n'`).
