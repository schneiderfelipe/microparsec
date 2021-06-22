import unittest

import streams

import microparsec
import microparsec/types

suite "basic manipulation":
  test "direct parser creation":
    check anyChar.debugParse("hello") == $('h', 1, 0, 1)
    check anyChar.debugParse(newStringStream("hello")) == anyChar.debugParse("hello")

    check anyChar.parse("hello") == ParseResult[char].ok 'h'
    check anyChar.parse(newStringStream("hello")) == anyChar.parse("hello")

    check ch('i').debugParse("hello") == $((unexpected: "\'h\'", expected: @["\'i\'"]), 0, 0, 0)
    check ch('i').debugParse(newStringStream("hello")) == ch('i').debugParse("hello")

    let s = newParseState("hello")
    check ch('i').parse(s) == ParseResult[char].err (unexpected: "\'h\'", expected: @["\'i\'"], state: s)
    check $ch('i').parse(s) == $ch('i').parse("hello")
    check $ch('i').parse(newStringStream("hello")) == $ch('i').parse("hello")

suite "parser algebra":
  test "functors":
    let p = anyChar
    let q = map(p) do (c: auto) -> auto:
      toUpperAscii(c)
    check q.debugParse("foo") == $('F', 1, 0, 1)
    check q.debugParse("oo") == $('O', 1, 0, 1)
    check q.debugParse("f") == $('F', 1, 0, 1)
    check q.debugParse("") == $((unexpected: "end of input", expected: @[
        "any character"]), 0, 0, 0)

    # First functor law
    check p.map(identity[char]).debugParse("foo") == p.debugParse("foo")

    # Second functor law
    let f = (c: char) => toUpperAscii(c)
    let g = (c: char) => toHex($c)
    check p.map(compose(f, g)).debugParse("foo") == p.map(f).map(g).debugParse("foo")

  test "applicatives":
    let p = anyChar
    let f: char -> char = toUpperAscii
    let q = pure(f) <*> p
    check q.debugParse("foo") == $('F', 1, 0, 1)
    check q.debugParse("oo") == $('O', 1, 0, 1)
    check q.debugParse("f") == $('F', 1, 0, 1)
    check q.debugParse("") == $((unexpected: "end of input", expected: @[
        "any character"]), 0, 0, 0)

    let selector: char -> (char -> (char -> (char, char))) = func(
        x: char): auto =
      return func(y: char): auto =
        return func(z: char): auto =
          (x, z)
    let dropMiddle = pure(selector) <*> anyChar <*> anyChar <*> anyChar
    check dropMiddle.debugParse("pumpkin") == $(('p', 'm'), 3, 0, 3)

  test "monads":
    let q = anyChar.flatMap do (c: char) -> auto:
      pure toUpperAscii(c)
    check q.debugParse("foo") == $('F', 1, 0, 1)
    check q.debugParse("oo") == $('O', 1, 0, 1)
    check q.debugParse("f") == $('F', 1, 0, 1)
    check q.debugParse("") == $((unexpected: "end of input", expected: @[
        "any character"]), 0, 0, 0)

    let dropMiddle = anyChar.flatMap do (x: char) -> auto:
      (anyChar >> anyChar).flatMap do (z: char) -> auto:
        pure (x, z)
    check dropMiddle.debugParse("pumpkin") == $(('p', 'm'), 3, 0, 3)
