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
    check p.parse("foo") == (ParseResult[char].ok 'f', 1)
    check p.parse("oo") == (ParseResult[char].ok 'o', 1)
    check p.parse("f") == (ParseResult[char].ok 'f', 1)
    check p.parse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["any character"]), 0)

  test "letter":
    let p = letter
    check p.parse("ello") == (ParseResult[char].ok 'e', 1)
    check p.parse("1hello") == (ParseResult[char].err (unexpected: "1", expected: @["letter"]), 0)
    check p.parse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["letter"]), 0)

  test "digit":
    let p = digit
    check p.parse("1hello") == (ParseResult[char].ok '1', 1)
    check p.parse("ello") == (ParseResult[char].err (unexpected: "e", expected: @["digit"]), 0)
    check p.parse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["digit"]), 0)

  # TODO: this always returns an empty string. This is in agreement with the
  # first Parsec paper, but it would be better to return the given string.
  test "str":
    let p = str("hello")
    # check p.parse("hello") == (ParseResult[string].ok "hello", 5)
    # check p.parse("hello world") == (ParseResult[string].ok "hello", 5)
    check p.parse("1hello") == (ParseResult[string].err (unexpected: "1", expected: @["hello"]), 0)
    check p.parse("ello") == (ParseResult[string].err (unexpected: "e", expected: @["hello"]), 0)
    check p.parse("") == (ParseResult[string].err (unexpected: "end of input", expected: @["hello"]), 0)

  # TODO: insert more examples where the results are other than
  # string/characters.
  test "many":
    let p = many(ch('h'))
    # Both `seq[char]` and `string` work! Very useful! But structural matching
    # does not work (such as comparing tuples and one of the fields are
    # seq[char]/string! We need to specialize some functions to return string
    # instead of seq[char], and get rid of all "newSeq[char]" everywhere.
    check p.parse("hello") == (ParseResult[seq[char]].ok @['h'], 1)
    check p.parse("hello") == (ParseResult[seq[char]].ok @['h'], 1)
    check p.parse("hhello") == (ParseResult[seq[char]].ok @['h', 'h'], 2)
    check p.parse("hhhello") == (ParseResult[seq[char]].ok @['h', 'h', 'h'], 3)
    check p.parse("ello") == (ParseResult[seq[char]].ok newSeq[char](), 0)
    check p.parse("") == (ParseResult[seq[char]].ok newSeq[char](), 0)

  # TODO: insert more examples where the results are other than
  # string/characters.
  test "many1":
    let p = many1(ch('h'))
    # Both `seq[char]` and `string` work! Very useful! But structural matching
    # does not work (such as comparing tuples and one of the fields are
    # seq[char]/string! We need to specialize some functions to return string
    # instead of seq[char], and get rid of all "newSeq[char]" everywhere.
    check p.parse("hello") == (ParseResult[seq[char]].ok @['h'], 1)
    check p.parse("hello") == (ParseResult[seq[char]].ok @['h'], 1)
    check p.parse("hhello") == (ParseResult[seq[char]].ok @['h', 'h'], 2)
    check p.parse("hhhello") == (ParseResult[seq[char]].ok @['h', 'h', 'h'], 3)
    check p.parse("ello") == (ParseResult[seq[char]].err (unexpected: "e", expected: @["h"]), 0)
    check p.parse("") == (ParseResult[seq[char]].err (unexpected: "end of input", expected: @["h"]), 0)

  # TODO: identifier is apparently not part of the standard Parsec and may be deleted in the future.
  test "identifier":
    let p = identifier
    # Both `seq[char]` and `string` work! Very useful! But structural matching
    # does not work (such as comparing tuples and one of the fields are
    # seq[char]/string! We need to specialize some functions to return string
    # instead of seq[char], and get rid of all "newSeq[char]" everywhere.
    check p.parse("hello") == (ParseResult[seq[char]].ok @['h', 'e', 'l', 'l', 'o'], 5)
    check p.parse("hello") == (ParseResult[seq[char]].ok @['h', 'e', 'l', 'l', 'o'], 5)
    check p.parse("hello world") == (ParseResult[seq[char]].ok @['h', 'e', 'l', 'l', 'o'], 5)
    check p.parse("123hello_ world") == (ParseResult[seq[char]].ok @['1', '2', '3', 'h', 'e', 'l', 'l', 'o', '_'], 9)
    check p.parse("*123hello_ world") == (ParseResult[seq[char]].err (unexpected: "*", expected: @["letter", "digit", "_"]), 0)
    check p.parse("") == (ParseResult[seq[char]].err (unexpected: "end of input", expected: @["letter", "digit", "_"]), 0)

  test "attempt":
    let p = attempt(ch('h'))
    check p.parse("hello") == (ParseResult[Option[char]].ok some('h'), 1)
    check p.parse("ello") == (ParseResult[Option[char]].ok none(char), 0)
    check p.parse("") == (ParseResult[Option[char]].ok none(char), 0)

    let q = attempt(ch('h') <|> ch('e'))
    check q.parse("hello") == (ParseResult[Option[char]].ok some('h'), 1)
    check q.parse("ello") == (ParseResult[Option[char]].ok some('e'), 1)
    check q.parse("") == (ParseResult[Option[char]].ok none(char), 0)

  test "pure":
    let p = pure('x')
    check p.parse("hello") == (ParseResult[char].ok 'x', 0)
    check p.parse("ello") == (ParseResult[char].ok 'x', 0)
    check p.parse("") == (ParseResult[char].ok 'x', 0)

  test "eof":
    let p = eof
    # Can't compare `ok`s due to a bug, see <https://github.com/arnetheduck/nim-result/issues/16>.
    # check p.parse("") == (ParseResult[void].ok, 0)
    check p.parse("")[0].isOk
    check p.parse("hello") == (ParseResult[void].err (unexpected: "h", expected: @["end of input"]), 0)

  test "ch":
    let p = ch('h')
    check p.parse("hello") == (ParseResult[char].ok 'h', 1)
    check p.parse("ello") == (ParseResult[char].err (unexpected: "e", expected: @["h"]), 0)
    check p.parse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["h"]), 0)

  test "between":
    # TODO: default errors for this combinator are not good enough.
    # Observe that the error message bypasses the possibility of more digits.
    # Think about the error messages as a set of tokens that would be required
    # to make the input valid.
    # TODO: the seq[char] thing is a real pain!
    let p = between(ch('{'), many(digit), ch('}'))
    check p.parse("{12}hello") == (ParseResult[seq[char]].ok @['1', '2'], 4)
    check p.parse("{}hello") == (ParseResult[seq[char]].ok newSeq[char](), 2)
    check p.parse("hello") == (ParseResult[seq[char]].err (unexpected: "h", expected: @["{"]), 0)
    check p.parse("{hello") == (ParseResult[seq[char]].err (unexpected: "h", expected: @["}"]), 1)
    check p.parse("{1hello") == (ParseResult[seq[char]].err (unexpected: "h", expected: @["}"]), 2)
    check p.parse("{12hello") == (ParseResult[seq[char]].err (unexpected: "h", expected: @["}"]), 3)
    check p.parse("") == (ParseResult[seq[char]].err (unexpected: "end of input", expected: @["{"]), 0)

    # Observe that the error message bypasses the possibility of more digits.
    # Think about the error messages as a set of tokens that would be required
    # to make the input valid.
    let q = between(ch('{'), many1(digit), ch('}'))
    check q.parse("{12}hello") == (ParseResult[seq[char]].ok @['1', '2'], 4)
    check q.parse("{}hello") == (ParseResult[seq[char]].err (unexpected: "}", expected: @["digit"]), 1)
    check q.parse("hello") == (ParseResult[seq[char]].err (unexpected: "h", expected: @["{"]), 0)
    check q.parse("{hello") == (ParseResult[seq[char]].err (unexpected: "h", expected: @["digit"]), 1)
    check q.parse("{1hello") == (ParseResult[seq[char]].err (unexpected: "h", expected: @["}"]), 2)
    check q.parse("{12hello") == (ParseResult[seq[char]].err (unexpected: "h", expected: @["}"]), 3)
    check q.parse("") == (ParseResult[seq[char]].err (unexpected: "end of input", expected: @["{"]), 0)

  test "sepBy":
    let p = sepBy(many1(digit), ch(','))
    check p.parse("1,2,3,4") == (ParseResult[seq[seq[char]]].ok @[@['1'], @['2'], @['3'], @['4']], 7)
    check p.parse("11,22") == (ParseResult[seq[seq[char]]].ok @[@['1', '1'], @['2', '2']], 5)
    # Observe how forgiving is that.
    check p.parse("11 ,22") == (ParseResult[seq[seq[char]]].ok @[@['1', '1']], 2)
    check p.parse("11, 22") == (ParseResult[seq[seq[char]]].ok @[@['1', '1']], 2)
    check p.parse("11,,22") == (ParseResult[seq[seq[char]]].ok @[@['1', '1']], 2)
    check p.parse(",") == (ParseResult[seq[seq[char]]].ok newSeq[seq[char]](), 0)
    check p.parse("") == (ParseResult[seq[seq[char]]].ok newSeq[seq[char]](), 0)
    # TODO: seq[seq[char]] is impossible to scale well. Although it is OK to
    # have seq[char] in place of string in many situations, higher order
    # containers start to scale bad. Solution: ensure we get
    #
    #         `seq[T]` for `T`,
    #     but `string` for `char`.
    #
    # check p.parse("1,2,3,4") == (ParseResult[seq[string]].ok @["1", "2", "3", "4"], 7)
    # check p.parse("") == (ParseResult[seq[string]].err (unexpected: "end of input", expected: @["{"]), 0)

  test "sepBy1":
    let p = sepBy1(many1(digit), ch(','))
    check p.parse("1,2,3,4") == (ParseResult[seq[seq[char]]].ok @[@['1'], @['2'], @['3'], @['4']], 7)
    check p.parse("11,22") == (ParseResult[seq[seq[char]]].ok @[@['1', '1'], @['2', '2']], 5)
    # Observe how forgiving is that.
    check p.parse("11 ,22") == (ParseResult[seq[seq[char]]].ok @[@['1', '1']], 2)
    check p.parse("11, 22") == (ParseResult[seq[seq[char]]].ok @[@['1', '1']], 2)
    check p.parse("11,,22") == (ParseResult[seq[seq[char]]].ok @[@['1', '1']], 2)
    check p.parse(",") == (ParseResult[seq[seq[char]]].err (unexpected: ",", expected: @["digit"]), 0)
    check p.parse("") == (ParseResult[seq[seq[char]]].err (unexpected: "end of input", expected: @["digit"]), 0)
    # TODO: seq[seq[char]] is impossible to scale well. Although it is OK to
    # have seq[char] in place of string in many situations, higher order
    # containers start to scale bad. Solution: ensure we get
    #
    #         `seq[T]` for `T`,
    #     but `string` for `char`.
    #
    # check p.parse("1,2,3,4") == (ParseResult[seq[string]].ok @["1", "2", "3", "4"], 7)
    # check p.parse("") == (ParseResult[seq[string]].err (unexpected: "end of input", expected: @["{"]), 0)

  test "optional":
    let p = optional(ch('h'))
    # check p.parse("ello") == (ParseResult[void].ok, 0)
    # check p.parse("hello") == (ParseResult[void].ok, 1)
    # check p.parse("hhello") == (ParseResult[void].ok, 2)
    # check p.parse("") == (ParseResult[void].ok, 0)
    check p.parse("ello")[0].isOk
    check p.parse("hello")[0].isOk
    check p.parse("hhello")[0].isOk
    check p.parse("")[0].isOk

suite "parsing utilities":
  test "parse":
    let p = anyChar
    check p.parse("hello") == (ParseResult[char].ok 'h', 1)
    check p.parse(newStringStream("hello")) == p.parse("hello")

suite "parser algebra":
  test "functors":
    # TODO: I might want to change the parameter order in the future. See
    # what is most common in the Nim world.
    let p = anyChar
    let q = fmap((c: char) => toUpperAscii(c), p)
    check q.parse("foo") == (ParseResult[char].ok 'F', 1)
    check q.parse("oo") == (ParseResult[char].ok 'O', 1)
    check q.parse("f") == (ParseResult[char].ok 'F', 1)
    check q.parse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["any character"]), 0)

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
    check q.parse("foo") == (ParseResult[char].ok 'F', 1)
    check q.parse("oo") == (ParseResult[char].ok 'O', 1)
    check q.parse("f") == (ParseResult[char].ok 'F', 1)
    check q.parse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["any character"]), 0)

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
    check dropMiddle.parse("pumpkin") == (ParseResult[(char, char)].ok ('p', 'm'), 3)

  test "monads":
    let p = anyChar
    let q = p >>= ((c: char) => pure(toUpperAscii(c)))
    check q.parse("foo") == (ParseResult[char].ok 'F', 1)
    check q.parse("oo") == (ParseResult[char].ok 'O', 1)
    check q.parse("f") == (ParseResult[char].ok 'F', 1)
    check q.parse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["any character"]), 0)

    # TODO: hey, something like Haskell's do-notation would make things like
    # that much nicer!
    let dropMiddle = anyChar >>= proc(x: char): auto =
      anyChar >>
        anyChar >>= func(z: char): auto =
          pure (x, z)
    check dropMiddle.parse("pumpkin") == (ParseResult[(char, char)].ok ('p', 'm'), 3)

suite "parser combinators":
  test "<|>":
    let p = ch('h') <|> ch('e')
    check p.parse("hello") == (ParseResult[char].ok 'h', 1)
    check p.parse("ehllo") == (ParseResult[char].ok 'e', 1)
    check p.parse("ello") == (ParseResult[char].ok 'e', 1)
    check p.parse("hllo") == (ParseResult[char].ok 'h', 1)
    check p.parse("llo") == (ParseResult[char].err (unexpected: "l", expected: @["h", "e"]), 0)
    check p.parse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["h", "e"]), 0)

  test ">>":
    # TODO: default errors for this combinator are not good enough.
    let p = ch('h') >> ch('e')
    check p.parse("hello") == (ParseResult[char].ok 'e', 2)
    check p.parse("ello") == (ParseResult[char].err (unexpected: "e", expected: @["h"]), 0)
    check p.parse("hllo") == (ParseResult[char].err (unexpected: "l", expected: @["e"]), 1)
    check p.parse("llo") == (ParseResult[char].err (unexpected: "l", expected: @["h"]), 0)
    check p.parse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["h"]), 0)

  test "*>":
    # TODO: default errors for this combinator are not good enough.
    let p = (ch('h') >> ch('e')) *> ch('l') >> ch('l')
    check p.parse("hello") == (ParseResult[char].ok 'l', 4)
    check p.parse("llo") == (ParseResult[char].ok 'l', 2)
    check p.parse("heklo") == (ParseResult[char].err (unexpected: "k", expected: @["l"]), 2)
    # check p.parse("ello") == (ParseResult[char].err (unexpected: "e", expected: @["h", "l"]), 0)
    # check p.parse("hllo") == (ParseResult[char].err (unexpected: "l", expected: @["e"]), 1)
    # check p.parse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["h", "l"]), 0)

  test "<*":
    let p = ch('a') <* ch('-')
    check p.parse("a-") == (ParseResult[char].ok 'a', 1)
    check p.parse("aa-") == (ParseResult[char].ok 'a', 1)
    check p.parse("-") == (ParseResult[char].err (unexpected: "-", expected: @["a"]), 0)
    check p.parse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["a"]), 0)

  test "<$":
    # TODO: default errors for this combinator are not good enough.
    let p = true <$ (ch('h') >> ch('e'))
    check p.parse("hello") == (ParseResult[bool].ok true, 2)
    check p.parse("ello") == (ParseResult[true].err (unexpected: "e", expected: @["h"]), 0)
    check p.parse("hllo") == (ParseResult[true].err (unexpected: "l", expected: @["e"]), 1)
    check p.parse("llo") == (ParseResult[true].err (unexpected: "l", expected: @["h"]), 0)
    check p.parse("") == (ParseResult[true].err (unexpected: "end of input", expected: @["h"]), 0)

suite "custom error messages":
  test "<?>":
    let p = "if" <$ (ch('i') >> ch('f')) <?> "if statement"
    check p.parse("if 1 > 0") == (ParseResult[string].ok "if", 2)
    check p.parse("f 1 > 0") == (ParseResult[string].err (unexpected: "f", expected: @["if statement"]), 0)
    check p.parse("") == (ParseResult[string].err (unexpected: "end of input", expected: @["if statement"]), 0)
