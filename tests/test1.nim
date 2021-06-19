# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import strutils
import unittest

import petametre
import petametre/primitives
import petametre/types

suite "basic character parsers":
  test "anyChar":
    let p = anyChar
    check p.parse("foo") == ParseResult[char].ok 'f'
    check p.parse("oo") == ParseResult[char].ok 'o'
    check p.parse("f") == ParseResult[char].ok 'f'
    check p.parse("") == ParseResult[char].err (position: 0, unexpected: "end of input", expected: @["any character"])

  test "letter":
    let p = letter
    check p.parse("ello") == ParseResult[char].ok 'e'
    check p.parse("1hello") == ParseResult[char].err (position: 0, unexpected: "1", expected: @["letter"])
    check p.parse("") == ParseResult[char].err (position: 0, unexpected: "end of input", expected: @["letter"])

  test "digit":
    let p = digit
    check p.parse("1hello") == ParseResult[char].ok '1'
    check p.parse("ello") == ParseResult[char].err (position: 0, unexpected: "e", expected: @["digit"])
    check p.parse("") == ParseResult[char].err (position: 0, unexpected: "end of input", expected: @["digit"])

  # TODO: this always returns an empty string. This is in agreement with the
  # first Parsec paper, but it would be better to return the given string.
  test "str":
    let p = str("hello")
    # check p.parse("hello") == ParseResult[string].ok "hello"
    # check p.parse("hello world") == ParseResult[string].ok "hello"
    check p.parse("1hello") == ParseResult[string].err (position: 0, unexpected: "1", expected: @["hello"])
    check p.parse("ello") == ParseResult[string].err (position: 0, unexpected: "e", expected: @["hello"])
    check p.parse("") == ParseResult[string].err (position: 0, unexpected: "end of input", expected: @["hello"])

  # TODO: insert more examples where the results are other than
  # string/characters.
  test "many":
    let p = many(ch('h'))
    # Both `seq[char]` and `string` work! Very useful!
    check p.parse("hello") == ParseResult[seq[char]].ok @['h']
    check p.parse("hello") == ParseResult[string].ok "h"
    check p.parse("hhello") == ParseResult[string].ok "hh"
    check p.parse("hhhello") == ParseResult[string].ok "hhh"
    check p.parse("ello") == ParseResult[string].ok ""
    check p.parse("") == ParseResult[string].ok ""

  # TODO: insert more examples where the results are other than
  # string/characters.
  test "many1":
    let p = many1(ch('h'))
    # Both `seq[char]` and `string` work! Very useful!
    check p.parse("hello") == ParseResult[seq[char]].ok @['h']
    check p.parse("hello") == ParseResult[string].ok "h"
    check p.parse("hhello") == ParseResult[string].ok "hh"
    check p.parse("hhhello") == ParseResult[string].ok "hhh"
    check p.parse("ello") == ParseResult[string].err (position: 0, unexpected: "e", expected: @["h"])
    check p.parse("") == ParseResult[string].err (position: 0, unexpected: "end of input", expected: @["h"])

  # TODO: identifier is apparently not part of the standard Parsec and may be deleted in the future.
  test "identifier":
    let p = identifier
    # Both `seq[char]` and `string` work! Very useful!
    check p.parse("hello") == ParseResult[seq[char]].ok @['h', 'e', 'l', 'l', 'o']
    check p.parse("hello") == ParseResult[string].ok "hello"
    check p.parse("hello world") == ParseResult[string].ok "hello"
    check p.parse("123hello_ world") == ParseResult[string].ok "123hello_"
    check p.parse("*123hello_ world") == ParseResult[string].err (position: 0, unexpected: "*", expected: @["letter", "digit", "_"])
    check p.parse("") == ParseResult[string].err (position: 0, unexpected: "end of input", expected: @["letter", "digit", "_"])

  test "attempt":
    let p = attempt(ch('h'))
    check p.parse("hello") == ParseResult[Option[char]].ok some('h')
    check p.parse("ello") == ParseResult[Option[char]].ok none(char)
    check p.parse("") == ParseResult[Option[char]].ok none(char)

    let q = attempt(ch('h') <|> ch('e'))
    check q.parse("hello") == ParseResult[Option[char]].ok some('h')
    check q.parse("ello") == ParseResult[Option[char]].ok some('e')
    check q.parse("") == ParseResult[Option[char]].ok none(char)

  test "pure":
    let p = pure('h')
    check p.parse("hello") == ParseResult[char].ok 'h'
    check p.parse("ello") == ParseResult[char].ok 'h'
    check p.parse("") == ParseResult[char].ok 'h'

  test "eof":
    let p = eof
    # Can't compare `ok`s due to a bug, see <https://github.com/arnetheduck/nim-result/issues/16>.
    # check p.parse("") == ParseResult[void].ok
    check p.parse("").isOk
    check p.parse("hello") == ParseResult[void].err (position: 0, unexpected: "h", expected: @["end of input"])

  test "ch":
    let p = ch('h')
    check p.parse("hello") == ParseResult[char].ok 'h'
    check p.parse("ello") == ParseResult[char].err (position: 0, unexpected: "e", expected: @["h"])
    check p.parse("") == ParseResult[char].err (position: 0, unexpected: "end of input", expected: @["h"])

  test "between":
    # Observe that the error message bypasses the possibility of more digits.
    # Think about the error messages as a set of tokens that would be required
    # to make the input valid.
    let p = between(ch('{'), many(digit), ch('}'))
    check p.parse("{12}hello") == ParseResult[string].ok "12"
    check p.parse("{}hello") == ParseResult[string].ok ""
    check p.parse("hello") == ParseResult[string].err (position: 0, unexpected: "h", expected: @["{"])
    check p.parse("{hello") == ParseResult[string].err (position: 1, unexpected: "h", expected: @["}"])
    check p.parse("{1hello") == ParseResult[string].err (position: 2, unexpected: "h", expected: @["}"])
    check p.parse("{12hello") == ParseResult[string].err (position: 3, unexpected: "h", expected: @["}"])
    check p.parse("") == ParseResult[string].err (position: 0, unexpected: "end of input", expected: @["{"])

    # Observe that the error message bypasses the possibility of more digits.
    # Think about the error messages as a set of tokens that would be required
    # to make the input valid.
    let q = between(ch('{'), many1(digit), ch('}'))
    check q.parse("{12}hello") == ParseResult[string].ok "12"
    check q.parse("{}hello") == ParseResult[string].err (position: 1, unexpected: "}", expected: @["digit"])
    check q.parse("hello") == ParseResult[string].err (position: 0, unexpected: "h", expected: @["{"])
    check q.parse("{hello") == ParseResult[string].err (position: 1, unexpected: "h", expected: @["digit"])
    check q.parse("{1hello") == ParseResult[string].err (position: 2, unexpected: "h", expected: @["}"])
    check q.parse("{12hello") == ParseResult[string].err (position: 3, unexpected: "h", expected: @["}"])
    check q.parse("") == ParseResult[string].err (position: 0, unexpected: "end of input", expected: @["{"])

  test "sepBy":
    let p = sepBy(many1(digit), ch(','))
    check p.parse("1,2,3,4") == ParseResult[seq[seq[char]]].ok @[@['1'], @['2'], @['3'], @['4']]
    check p.parse("11,22") == ParseResult[seq[seq[char]]].ok @[@['1', '1'], @['2', '2']]
    # Observe how forgiving is that.
    check p.parse("11 ,22") == ParseResult[seq[seq[char]]].ok @[@['1', '1']]
    check p.parse("11, 22") == ParseResult[seq[seq[char]]].ok @[@['1', '1']]
    check p.parse("11,,22") == ParseResult[seq[seq[char]]].ok @[@['1', '1']]
    check p.parse(",") == ParseResult[seq[seq[char]]].ok newSeq[seq[char]]()
    check p.parse("") == ParseResult[seq[seq[char]]].ok newSeq[seq[char]]()
    # TODO: seq[seq[char]] is impossible to scale well. Although it is OK to
    # have seq[char] in place of string in many situations, higher order
    # containers start to scale bad. Solution: ensure we get
    #
    #         `seq[T]` for `T`,
    #     but `string` for `char`.
    #
    # check p.parse("1,2,3,4") == ParseResult[seq[string]].ok @["1", "2", "3", "4"]
    # check p.parse("") == ParseResult[seq[string]].err (position: 0, unexpected: "end of input", expected: @["{"])

  test "sepBy1":
    let p = sepBy1(many1(digit), ch(','))
    check p.parse("1,2,3,4") == ParseResult[seq[seq[char]]].ok @[@['1'], @['2'], @['3'], @['4']]
    check p.parse("11,22") == ParseResult[seq[seq[char]]].ok @[@['1', '1'], @['2', '2']]
    # Observe how forgiving is that.
    check p.parse("11 ,22") == ParseResult[seq[seq[char]]].ok @[@['1', '1']]
    check p.parse("11, 22") == ParseResult[seq[seq[char]]].ok @[@['1', '1']]
    check p.parse("11,,22") == ParseResult[seq[seq[char]]].ok @[@['1', '1']]
    check p.parse(",") == ParseResult[seq[seq[char]]].err (position: 0, unexpected: ",", expected: @["digit"])
    check p.parse("") == ParseResult[seq[seq[char]]].err (position: 0, unexpected: "end of input", expected: @["digit"])
    # TODO: seq[seq[char]] is impossible to scale well. Although it is OK to
    # have seq[char] in place of string in many situations, higher order
    # containers start to scale bad. Solution: ensure we get
    #
    #         `seq[T]` for `T`,
    #     but `string` for `char`.
    #
    # check p.parse("1,2,3,4") == ParseResult[seq[string]].ok @["1", "2", "3", "4"]
    # check p.parse("") == ParseResult[seq[string]].err (position: 0, unexpected: "end of input", expected: @["{"])

  test "optional":
    let p = optional(ch('h'))
    # check p.parse("ello") == ParseResult[void].ok
    # check p.parse("hello") == ParseResult[void].ok
    # check p.parse("hhello") == ParseResult[void].ok
    # check p.parse("") == ParseResult[void].ok
    check p.parse("ello").isOk
    check p.parse("hello").isOk
    check p.parse("hhello").isOk
    check p.parse("").isOk

suite "parsing utilities":
  test "parse":
    let p = anyChar
    check p.parse("hello") == ParseResult[char].ok 'h'
    check p.parse(newStringStream("hello")) == p.parse("hello")

suite "parser algebra":
  test "functors":
    # TODO: I might want to change the parameter order in the future. See
    # what is most common in the Nim world.
    let p = anyChar
    let q = fmap((c: char) => toUpperAscii(c), p)
    check q.parse("foo") == ParseResult[char].ok 'F'
    check q.parse("oo") == ParseResult[char].ok 'O'
    check q.parse("f") == ParseResult[char].ok 'F'
    check q.parse("") == ParseResult[char].err (position: 0, unexpected: "end of input", expected: @["any character"])

    # First functor law
    check fmap(identity[char], p).parse("foo") == p.parse("foo")

    # Second functor law
    let f = (c: char) => toUpperAscii(c)
    let g = (c: char) => toHex($c)
    check fmap(compose(f, g), p).parse("foo") == fmap(g, fmap(f, p)).parse("foo")

  test "applicatives":
    let p = anyChar
    let f: char -> char = toUpperAscii
    let q = pure(f) <*> p
    check q.parse("foo") == ParseResult[char].ok 'F'
    check q.parse("oo") == ParseResult[char].ok 'O'
    check q.parse("f") == ParseResult[char].ok 'F'
    check q.parse("") == ParseResult[char].err (position: 0, unexpected: "end of input", expected: @["any character"])

    # Poor man's currying and a lot of help to the compiler
    # TODO: hey, something like true currying would make things like that much
    # nicer!
    # TODO: or we could use lift and varargs for greater good! Choose what is
    # simpler and nimbler!
    let selector: char -> (char -> (char -> (char, char))) = func(x: char): auto =
      return func(y: char): auto =
        return func(z: char): auto =
          (x, z)
    let dropMiddle = pure(selector) <*> anyChar <*> anyChar <*> anyChar
    check dropMiddle.parse("pumpkin") == ParseResult[(char, char)].ok ('p', 'm')

  test "monads":
    let p = anyChar
    let q = p >>= ((c: char) => pure(toUpperAscii(c)))
    check q.parse("foo") == ParseResult[char].ok 'F'
    check q.parse("oo") == ParseResult[char].ok 'O'
    check q.parse("f") == ParseResult[char].ok 'F'
    check q.parse("") == ParseResult[char].err (position: 0, unexpected: "end of input", expected: @["any character"])

    # TODO: hey, something like Haskell's do-notation would make things like
    # that much nicer!
    let dropMiddle = anyChar >>= proc(x: char): auto =
      anyChar >>
        anyChar >>= func(z: char): auto =
          pure (x, z)
    check dropMiddle.parse("pumpkin") == ParseResult[(char, char)].ok ('p', 'm')

suite "parser combinators":
  test "<|>":
    let p = ch('h') <|> ch('e')
    check p.parse("hello") == ParseResult[char].ok 'h'
    check p.parse("ehllo") == ParseResult[char].ok 'e'
    check p.parse("ello") == ParseResult[char].ok 'e'
    check p.parse("hllo") == ParseResult[char].ok 'h'
    check p.parse("llo") == ParseResult[char].err (position: 0, unexpected: "l", expected: @["h", "e"])
    check p.parse("") == ParseResult[char].err (position: 0, unexpected: "end of input", expected: @["h", "e"])

  test ">>":
    let p = ch('h') >> ch('e')
    check p.parse("hello") == ParseResult[char].ok 'e'
    check p.parse("ello") == ParseResult[char].err (position: 0, unexpected: "e", expected: @["h"])
    check p.parse("hllo") == ParseResult[char].err (position: 1, unexpected: "l", expected: @["e"])
    check p.parse("llo") == ParseResult[char].err (position: 0, unexpected: "l", expected: @["h"])
    check p.parse("") == ParseResult[char].err (position: 0, unexpected: "end of input", expected: @["h"])

  test "*>":
    # TODO: default errors for this combinator are not good enough.
    let p = (ch('h') >> ch('e')) *> ch('l') >> ch('l')
    check p.parse("hello") == ParseResult[char].ok 'l'
    check p.parse("llo") == ParseResult[char].ok 'l'
    check p.parse("heklo") == ParseResult[char].err (position: 2, unexpected: "k", expected: @["l"])
    # check p.parse("ello") == ParseResult[char].err (position: 0, unexpected: "e", expected: @["h", "l"])
    # check p.parse("hllo") == ParseResult[char].err (position: 0, unexpected: "l", expected: @["e"])
    # check p.parse("") == ParseResult[char].err (position: 0, unexpected: "end of input", expected: @["h", "l"])

  test "<*":
    let p = ch('a') <* ch('-')
    check p.parse("a-") == ParseResult[char].ok 'a'
    check p.parse("aa-") == ParseResult[char].ok 'a'
    check p.parse("-") == ParseResult[char].err (position: 0, unexpected: "-", expected: @["a"])
    check p.parse("") == ParseResult[char].err (position: 0, unexpected: "end of input", expected: @["a"])

  test "<$":
    let p = true <$ (ch('h') >> ch('e'))
    check p.parse("hello") == ParseResult[bool].ok true
    check p.parse("ello") == ParseResult[true].err (position: 0, unexpected: "e", expected: @["h"])
    check p.parse("hllo") == ParseResult[true].err (position: 1, unexpected: "l", expected: @["e"])
    check p.parse("llo") == ParseResult[true].err (position: 0, unexpected: "l", expected: @["h"])
    check p.parse("") == ParseResult[true].err (position: 0, unexpected: "end of input", expected: @["h"])

suite "custom error messages":
  test "<?>":
    let p = "if" <$ (ch('i') >> ch('f')) <?> "if statement"
    check p.parse("if 1 > 0") == ParseResult[string].ok "if"
    check p.parse("f 1 > 0") == ParseResult[string].err (position: 0, unexpected: "f", expected: @["if statement"])
    check p.parse("") == ParseResult[string].err (position: 0, unexpected: "end of input", expected: @["if statement"])
