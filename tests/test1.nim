# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest

import streams
import strutils

import petametre
import petametre/primitives
import petametre/types

suite "basic character parsers":
  test "anyChar":
    let p = anyChar
    check p.debugParse("foo") == (ParseResult[char].ok 'f', 1, 0, 1)
    check p.debugParse("oo") == (ParseResult[char].ok 'o', 1, 0, 1)
    check p.debugParse("f") == (ParseResult[char].ok 'f', 1, 0, 1)
    check p.debugParse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["any character"]), 0, 0, 0)

  test "letter":
    let p = letter
    check p.debugParse("ello") == (ParseResult[char].ok 'e', 1, 0, 1)
    check p.debugParse("1hello") == (ParseResult[char].err (unexpected: "1", expected: @["letter"]), 0, 0, 0)
    check p.debugParse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["letter"]), 0, 0, 0)

  test "digit":
    let p = digit
    check p.debugParse("1hello") == (ParseResult[char].ok '1', 1, 0, 1)
    check p.debugParse("ello") == (ParseResult[char].err (unexpected: "e", expected: @["digit"]), 0, 0, 0)
    check p.debugParse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["digit"]), 0, 0, 0)

  # TODO: this always returns an empty string. This is in agreement with the
  # first Parsec paper, but it would be better to return the given string.
  test "str":
    let p = str("hello")
    # check p.debugParse("hello") == (ParseResult[string].ok "hello", 5, 0, 5)
    # check p.debugParse("hello world") == (ParseResult[string].ok "hello", 5, 0, 5)
    check p.debugParse("1hello") == (ParseResult[string].err (unexpected: "1", expected: @["hello"]), 0, 0, 0)
    check p.debugParse("ello") == (ParseResult[string].err (unexpected: "e", expected: @["hello"]), 0, 0, 0)
    check p.debugParse("") == (ParseResult[string].err (unexpected: "end of input", expected: @["hello"]), 0, 0, 0)

  # TODO: insert more examples where the results are other than
  # string/characters.
  test "many":
    let p = many(ch('h'))
    # Both `seq[char]` and `string` work! Very useful! But structural matching
    # does not work (such as comparing tuples and one of the fields are
    # seq[char]/string! We need to specialize some functions to return string
    # instead of seq[char], and get rid of all "newSeq[char]" everywhere.
    check p.debugParse("hello") == (ParseResult[seq[char]].ok @['h'], 1, 0, 1)
    check p.debugParse("hello") == (ParseResult[seq[char]].ok @['h'], 1, 0, 1)
    check p.debugParse("hhello") == (ParseResult[seq[char]].ok @['h', 'h'], 2, 0, 2)
    check p.debugParse("hhhello") == (ParseResult[seq[char]].ok @['h', 'h', 'h'], 3, 0, 3)
    check p.debugParse("ello") == (ParseResult[seq[char]].ok newSeq[char](), 0, 0, 0)
    check p.debugParse("") == (ParseResult[seq[char]].ok newSeq[char](), 0, 0, 0)

  # TODO: insert more examples where the results are other than
  # string/characters.
  test "many1":
    let p = many1(ch('h'))
    # Both `seq[char]` and `string` work! Very useful! But structural matching
    # does not work (such as comparing tuples and one of the fields are
    # seq[char]/string! We need to specialize some functions to return string
    # instead of seq[char], and get rid of all "newSeq[char]" everywhere.
    check p.debugParse("hello") == (ParseResult[seq[char]].ok @['h'], 1, 0, 1)
    check p.debugParse("hello") == (ParseResult[seq[char]].ok @['h'], 1, 0, 1)
    check p.debugParse("hhello") == (ParseResult[seq[char]].ok @['h', 'h'], 2, 0, 2)
    check p.debugParse("hhhello") == (ParseResult[seq[char]].ok @['h', 'h', 'h'], 3, 0, 3)
    check p.debugParse("ello") == (ParseResult[seq[char]].err (unexpected: "e", expected: @["h"]), 0, 0, 0)
    check p.debugParse("") == (ParseResult[seq[char]].err (unexpected: "end of input", expected: @["h"]), 0, 0, 0)

  # TODO: identifier is apparently not part of the standard Parsec and may be deleted in the future.
  test "identifier":
    let p = identifier
    # Both `seq[char]` and `string` work! Very useful! But structural matching
    # does not work (such as comparing tuples and one of the fields are
    # seq[char]/string! We need to specialize some functions to return string
    # instead of seq[char], and get rid of all "newSeq[char]" everywhere.
    check p.debugParse("hello") == (ParseResult[seq[char]].ok @['h', 'e', 'l', 'l', 'o'], 5, 0, 5)
    check p.debugParse("hello") == (ParseResult[seq[char]].ok @['h', 'e', 'l', 'l', 'o'], 5, 0, 5)
    check p.debugParse("hello world") == (ParseResult[seq[char]].ok @['h', 'e', 'l', 'l', 'o'], 5, 0, 5)
    check p.debugParse("123hello_ world") == (ParseResult[seq[char]].ok @['1', '2', '3', 'h', 'e', 'l', 'l', 'o', '_'], 9, 0, 9)
    check p.debugParse("*123hello_ world") == (ParseResult[seq[char]].err (unexpected: "*", expected: @["letter", "digit", "_"]), 0, 0, 0)
    check p.debugParse("") == (ParseResult[seq[char]].err (unexpected: "end of input", expected: @["letter", "digit", "_"]), 0, 0, 0)

  test "attempt":
    let p = attempt(ch('h'))
    check p.debugParse("hello") == (ParseResult[Option[char]].ok some('h'), 1, 0, 1)
    check p.debugParse("ello") == (ParseResult[Option[char]].ok none(char), 0, 0, 0)
    check p.debugParse("") == (ParseResult[Option[char]].ok none(char), 0, 0, 0)

    let q = attempt(ch('h') <|> ch('e'))
    check q.debugParse("hello") == (ParseResult[Option[char]].ok some('h'), 1, 0, 1)
    check q.debugParse("ello") == (ParseResult[Option[char]].ok some('e'), 1, 0, 1)
    check q.debugParse("") == (ParseResult[Option[char]].ok none(char), 0, 0, 0)

  test "pure":
    let p = pure('x')
    check p.debugParse("hello") == (ParseResult[char].ok 'x', 0, 0, 0)
    check p.debugParse("ello") == (ParseResult[char].ok 'x', 0, 0, 0)
    check p.debugParse("") == (ParseResult[char].ok 'x', 0, 0, 0)

  test "eof":
    let p = eof
    # Can't compare `ok`s due to a bug, see <https://github.com/arnetheduck/nim-result/issues/16>.
    # check p.debugParse("") == (ParseResult[void].ok, 0, 0, 0)
    check p.debugParse("")[0].isOk
    check p.debugParse("hello") == (ParseResult[void].err (unexpected: "h", expected: @["end of input"]), 0, 0, 0)

  test "ch":
    let p = ch('h')
    check p.debugParse("hello") == (ParseResult[char].ok 'h', 1, 0, 1)
    check p.debugParse("ello") == (ParseResult[char].err (unexpected: "e", expected: @["h"]), 0, 0, 0)
    check p.debugParse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["h"]), 0, 0, 0)

  test "between":
    # TODO: default errors for this combinator are not good enough.
    # Observe that the error message bypasses the possibility of more digits.
    # Think about the error messages as a set of tokens that would be required
    # to make the input valid.
    # TODO: the seq[char] thing is a real pain!
    let p = between(ch('{'), many(digit), ch('}'))
    check p.debugParse("{12}hello") == (ParseResult[seq[char]].ok @['1', '2'], 4, 0, 4)
    check p.debugParse("{}hello") == (ParseResult[seq[char]].ok newSeq[char](), 2, 0, 2)
    check p.debugParse("hello") == (ParseResult[seq[char]].err (unexpected: "h", expected: @["{"]), 0, 0, 0)
    check p.debugParse("{hello") == (ParseResult[seq[char]].err (unexpected: "h", expected: @["}"]), 1, 0, 1)
    check p.debugParse("{1hello") == (ParseResult[seq[char]].err (unexpected: "h", expected: @["}"]), 2, 0, 2)
    check p.debugParse("{12hello") == (ParseResult[seq[char]].err (unexpected: "h", expected: @["}"]), 3, 0, 3)
    check p.debugParse("") == (ParseResult[seq[char]].err (unexpected: "end of input", expected: @["{"]), 0, 0, 0)

    # Observe that the error message bypasses the possibility of more digits.
    # Think about the error messages as a set of tokens that would be required
    # to make the input valid.
    let q = between(ch('{'), many1(digit), ch('}'))
    check q.debugParse("{12}hello") == (ParseResult[seq[char]].ok @['1', '2'], 4, 0, 4)
    check q.debugParse("{}hello") == (ParseResult[seq[char]].err (unexpected: "}", expected: @["digit"]), 1, 0, 1)
    check q.debugParse("hello") == (ParseResult[seq[char]].err (unexpected: "h", expected: @["{"]), 0, 0, 0)
    check q.debugParse("{hello") == (ParseResult[seq[char]].err (unexpected: "h", expected: @["digit"]), 1, 0, 1)
    check q.debugParse("{1hello") == (ParseResult[seq[char]].err (unexpected: "h", expected: @["}"]), 2, 0, 2)
    check q.debugParse("{12hello") == (ParseResult[seq[char]].err (unexpected: "h", expected: @["}"]), 3, 0, 3)
    check q.debugParse("") == (ParseResult[seq[char]].err (unexpected: "end of input", expected: @["{"]), 0, 0, 0)

  test "sepBy":
    let p = sepBy(many1(digit), ch(','))
    check p.debugParse("1,2,3,4") == (ParseResult[seq[seq[char]]].ok @[@['1'], @['2'], @['3'], @['4']], 7, 0, 7)
    check p.debugParse("11,22") == (ParseResult[seq[seq[char]]].ok @[@['1', '1'], @['2', '2']], 5, 0, 5)
    # Observe how forgiving is that. Also observe how greedy that is.
    check p.debugParse("11 ,22") == (ParseResult[seq[seq[char]]].ok @[@['1', '1']], 2, 0, 2)
    check p.debugParse("11, 22") == (ParseResult[seq[seq[char]]].ok @[@['1', '1']], 3, 0, 3)
    check p.debugParse("11,,22") == (ParseResult[seq[seq[char]]].ok @[@['1', '1']], 3, 0, 3)
    check p.debugParse(",") == (ParseResult[seq[seq[char]]].ok newSeq[seq[char]](), 0, 0, 0)
    check p.debugParse("") == (ParseResult[seq[seq[char]]].ok newSeq[seq[char]](), 0, 0, 0)
    # TODO: seq[seq[char]] is impossible to scale well. Although it is OK to
    # have seq[char] in place of string in many situations, higher order
    # containers start to scale bad. Solution: ensure we get
    #
    #         `seq[T]` for `T`,
    #     but `string` for `char`.
    #
    # check p.debugParse("1,2,3,4") == (ParseResult[seq[string]].ok @["1", "2", "3", "4"], 7, 0, 7)
    # check p.debugParse("") == (ParseResult[seq[string]].err (unexpected: "end of input", expected: @["{"]), 0, 0, 0)


    # If you think sepBy should not be eager, think again: it should. See
    # <https://github.com/mrkkrp/megaparsec/issues/401#issue-572499736>,
    # whose example is reproduced below.
    func foo[R,S,T](p: Parser[R], sep: Parser[S], q: Parser[T]): Parser[void] =
      sepBy(p, sep) >> optional(sep >> q)
    let res = foo(str("a"), str(" "), str("b")).debugParse("a a b")
    check res[0].isOk
    check res[1] == 4

  test "sepBy1":
    let p = sepBy1(many1(digit), ch(','))
    check p.debugParse("1,2,3,4") == (ParseResult[seq[seq[char]]].ok @[@['1'], @['2'], @['3'], @['4']], 7, 0, 7)
    check p.debugParse("11,22") == (ParseResult[seq[seq[char]]].ok @[@['1', '1'], @['2', '2']], 5, 0, 5)
    # Observe how forgiving is that. Also observe how greedy that is.
    check p.debugParse("11 ,22") == (ParseResult[seq[seq[char]]].ok @[@['1', '1']], 2, 0, 2)
    check p.debugParse("11, 22") == (ParseResult[seq[seq[char]]].ok @[@['1', '1']], 3, 0, 3)
    check p.debugParse("11,,22") == (ParseResult[seq[seq[char]]].ok @[@['1', '1']], 3, 0, 3)
    check p.debugParse(",") == (ParseResult[seq[seq[char]]].err (unexpected: ",", expected: @["digit"]), 0, 0, 0)
    check p.debugParse("") == (ParseResult[seq[seq[char]]].err (unexpected: "end of input", expected: @["digit"]), 0, 0, 0)
    # TODO: seq[seq[char]] is impossible to scale well. Although it is OK to
    # have seq[char] in place of string in many situations, higher order
    # containers start to scale bad. Solution: ensure we get
    #
    #         `seq[T]` for `T`,
    #     but `string` for `char`.
    #
    # check p.debugParse("1,2,3,4") == (ParseResult[seq[string]].ok @["1", "2", "3", "4"], 7, 0, 7)
    # check p.debugParse("") == (ParseResult[seq[string]].err (unexpected: "end of input", expected: @["{"]), 0, 0, 0)

  test "optional":
    let p = optional(ch('h'))
    # check p.debugParse("ello") == (ParseResult[void].ok, 0, 0, 0)
    # check p.debugParse("hello") == (ParseResult[void].ok, 1, 0, 1)
    # check p.debugParse("hhello") == (ParseResult[void].ok, 2, 0, 2)
    # check p.debugParse("") == (ParseResult[void].ok, 0, 0, 0)
    check p.debugParse("ello")[0].isOk
    check p.debugParse("hello")[0].isOk
    check p.debugParse("hhello")[0].isOk
    check p.debugParse("")[0].isOk

suite "parsing utilities":
  test "parse":
    let p = anyChar
    check p.debugParse("hello") == (ParseResult[char].ok 'h', 1, 0, 1)
    check p.debugParse(newStringStream("hello")) == p.debugParse("hello")

    check p.parse("hello") == ParseResult[char].ok 'h'
    check p.parse(newStringStream("hello")) == p.parse("hello")

  test "position state":
    let p = anyChar >> anyChar >> anyChar >> anyChar >> anyChar
    check p.debugParse("foo") == (ParseResult[char].err (unexpected: "end of input", expected: @["any character"]), 3, 0, 3)
    check p.debugParse("fooo") == (ParseResult[char].err (unexpected: "end of input", expected: @["any character"]), 4, 0, 4)
    check p.debugParse("foooo") == (ParseResult[char].ok 'o', 5, 0, 5)

    check p.debugParse("\nfoo") == (ParseResult[char].err (unexpected: "end of input", expected: @["any character"]), 4, 1, 3)
    check p.debugParse("f\noo") == (ParseResult[char].err (unexpected: "end of input", expected: @["any character"]), 4, 1, 2)
    check p.debugParse("fo\no") == (ParseResult[char].err (unexpected: "end of input", expected: @["any character"]), 4, 1, 1)
    check p.debugParse("foo\n") == (ParseResult[char].err (unexpected: "end of input", expected: @["any character"]), 4, 0, 4)

    check p.debugParse("\n\nfoo") == (ParseResult[char].ok 'o', 5, 2, 3)
    check p.debugParse("\nf\noo") == (ParseResult[char].ok 'o', 5, 2, 2)
    check p.debugParse("\nfo\no") == (ParseResult[char].ok 'o', 5, 2, 1)
    check p.debugParse("\nfoo\n") == (ParseResult[char].ok '\n', 5, 1, 4)

    check p.debugParse("\nfooo") == (ParseResult[char].ok 'o', 5, 1, 4)
    check p.debugParse("f\nooo") == (ParseResult[char].ok 'o', 5, 1, 3)
    check p.debugParse("fo\noo") == (ParseResult[char].ok 'o', 5, 1, 2)
    check p.debugParse("foo\no") == (ParseResult[char].ok 'o', 5, 1, 1)
    check p.debugParse("fooo\n") == (ParseResult[char].ok '\n', 5, 0, 5)  # Newline belongs to previous line

    check p.debugParse("\n\nfooo") == (ParseResult[char].ok 'o', 5, 2, 3)
    check p.debugParse("\nf\nooo") == (ParseResult[char].ok 'o', 5, 2, 2)
    check p.debugParse("\nfo\noo") == (ParseResult[char].ok 'o', 5, 2, 1)
    check p.debugParse("\nfoo\no") == (ParseResult[char].ok '\n', 5, 1, 4)
    check p.debugParse("\nfooo\n") == (ParseResult[char].ok 'o', 5, 1, 4)

    check p.debugParse("\nfo\noo") == (ParseResult[char].ok 'o', 5, 2, 1)
    check p.debugParse("f\no\noo") == (ParseResult[char].ok 'o', 5, 2, 1)
    check p.debugParse("fo\n\noo") == (ParseResult[char].ok 'o', 5, 2, 1)
    check p.debugParse("foo\n\no") == (ParseResult[char].ok '\n', 5, 1, 0)
    check p.debugParse("foo\no\n") == (ParseResult[char].ok 'o', 5, 1, 1)

    check p.debugParse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["any character"]), 0, 0, 0)

suite "parser algebra":
  test "functors":
    # TODO: I might want to change the parameter order in the future. See
    # what is most common in the Nim world.
    let p = anyChar
    let q = fmap((c: char) => toUpperAscii(c), p)
    check q.debugParse("foo") == (ParseResult[char].ok 'F', 1, 0, 1)
    check q.debugParse("oo") == (ParseResult[char].ok 'O', 1, 0, 1)
    check q.debugParse("f") == (ParseResult[char].ok 'F', 1, 0, 1)
    check q.debugParse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["any character"]), 0, 0, 0)

    # First functor law
    check fmap(identity[char], p).debugParse("foo") == p.debugParse("foo")

    # Second functor law
    let f = (c: char) => toUpperAscii(c)
    let g = (c: char) => toHex($c)
    check fmap(compose(f, g), p).debugParse("foo") == fmap(g, fmap(f, p)).debugParse("foo")

  test "applicatives":
    let p = anyChar
    let f: char -> char = toUpperAscii
    let q = pure(f) <*> p
    check q.debugParse("foo") == (ParseResult[char].ok 'F', 1, 0, 1)
    check q.debugParse("oo") == (ParseResult[char].ok 'O', 1, 0, 1)
    check q.debugParse("f") == (ParseResult[char].ok 'F', 1, 0, 1)
    check q.debugParse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["any character"]), 0, 0, 0)

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
    check dropMiddle.debugParse("pumpkin") == (ParseResult[(char, char)].ok ('p', 'm'), 3, 0, 3)

  test "monads":
    let p = anyChar
    let q = p >>= ((c: char) => pure(toUpperAscii(c)))
    check q.debugParse("foo") == (ParseResult[char].ok 'F', 1, 0, 1)
    check q.debugParse("oo") == (ParseResult[char].ok 'O', 1, 0, 1)
    check q.debugParse("f") == (ParseResult[char].ok 'F', 1, 0, 1)
    check q.debugParse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["any character"]), 0, 0, 0)

    # TODO: hey, something like Haskell's do-notation would make things like
    # that much nicer!
    let dropMiddle = anyChar >>= proc(x: char): auto =
      anyChar >>
        anyChar >>= func(z: char): auto =
          pure (x, z)
    check dropMiddle.debugParse("pumpkin") == (ParseResult[(char, char)].ok ('p', 'm'), 3, 0, 3)

suite "parser combinators":
  test "<|>":
    let p = ch('h') <|> ch('e')
    check p.debugParse("hello") == (ParseResult[char].ok 'h', 1, 0, 1)
    check p.debugParse("ehllo") == (ParseResult[char].ok 'e', 1, 0, 1)
    check p.debugParse("ello") == (ParseResult[char].ok 'e', 1, 0, 1)
    check p.debugParse("hllo") == (ParseResult[char].ok 'h', 1, 0, 1)
    check p.debugParse("llo") == (ParseResult[char].err (unexpected: "l", expected: @["h", "e"]), 0, 0, 0)
    check p.debugParse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["h", "e"]), 0, 0, 0)

  test ">>":
    # TODO: default errors for this combinator are not good enough.
    let p = ch('h') >> ch('e')
    check p.debugParse("hello") == (ParseResult[char].ok 'e', 2, 0, 2)
    check p.debugParse("ello") == (ParseResult[char].err (unexpected: "e", expected: @["h"]), 0, 0, 0)
    check p.debugParse("hllo") == (ParseResult[char].err (unexpected: "l", expected: @["e"]), 1, 0, 1)
    check p.debugParse("llo") == (ParseResult[char].err (unexpected: "l", expected: @["h"]), 0, 0, 0)
    check p.debugParse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["h"]), 0, 0, 0)

  test "*>":
    # TODO: default errors for this combinator are not good enough.
    let p = (ch('h') >> ch('e')) *> ch('l') >> ch('l')
    check p.debugParse("hello") == (ParseResult[char].ok 'l', 4, 0, 4)
    check p.debugParse("llo") == (ParseResult[char].ok 'l', 2, 0, 2)
    check p.debugParse("heklo") == (ParseResult[char].err (unexpected: "k", expected: @["l"]), 2, 0, 2)
    # check p.debugParse("ello") == (ParseResult[char].err (unexpected: "e", expected: @["h", "l"]), 0, 0, 0)
    # check p.debugParse("hllo") == (ParseResult[char].err (unexpected: "l", expected: @["e"]), 1, 0, 1)
    # check p.debugParse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["h", "l"]), 0, 0, 0)

  test "<*":
    let p = ch('a') <* ch('-')
    check p.debugParse("a-") == (ParseResult[char].ok 'a', 2, 0, 2)
    check p.debugParse("aa-") == (ParseResult[char].ok 'a', 1, 0, 1)
    check p.debugParse("b-") == (ParseResult[char].err (unexpected: "b", expected: @["a"]), 0, 0, 0)
    check p.debugParse("-") == (ParseResult[char].err (unexpected: "-", expected: @["a"]), 0, 0, 0)
    check p.debugParse("") == (ParseResult[char].err (unexpected: "end of input", expected: @["a"]), 0, 0, 0)

  test "<$":
    # TODO: default errors for this combinator are not good enough.
    let p = true <$ (ch('h') >> ch('e'))
    check p.debugParse("hello") == (ParseResult[bool].ok true, 2, 0, 2)
    check p.debugParse("ello") == (ParseResult[true].err (unexpected: "e", expected: @["h"]), 0, 0, 0)
    check p.debugParse("hllo") == (ParseResult[true].err (unexpected: "l", expected: @["e"]), 1, 0, 1)
    check p.debugParse("llo") == (ParseResult[true].err (unexpected: "l", expected: @["h"]), 0, 0, 0)
    check p.debugParse("") == (ParseResult[true].err (unexpected: "end of input", expected: @["h"]), 0, 0, 0)

suite "custom error messages":
  test "<?>":
    let p = "if" <$ (ch('i') >> ch('f')) <?> "if statement"
    check p.debugParse("if 1 > 0") == (ParseResult[string].ok "if", 2, 0, 2)
    check p.debugParse("f 1 > 0") == (ParseResult[string].err (unexpected: "f", expected: @["if statement"]), 0, 0, 0)
    check p.debugParse("") == (ParseResult[string].err (unexpected: "end of input", expected: @["if statement"]), 0, 0, 0)
