import unittest

import microparsec

suite "single character parsers":
  test "satisfy":
    let p = (satisfy do (c: char) -> auto:
      c in {'+', '-', '*', '/'}) <?> "operation"
    check p.debugParse("+") == $('+', 1, 0, 1)
    check p.debugParse("1") == $((unexpected: "\'1\'", expected: @[
        "operation"]), 0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "operation"]), 0, 0, 0)

  test "skip":
    let p = (skip do (c: char) -> auto:
      c in {'+', '-', '*', '/'}) <?> "skippable operation"
    check p.debugParse("+") == $(1, 0, 1)
    check p.debugParse("1") == $((unexpected: "\'1\'", expected: @[
        "skippable operation"]), 0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "skippable operation"]), 0, 0, 0)

  test "satisfyWith":
    let p = (satisfyWith(c => ord(c)) do (x: auto) -> auto:
      x < 97) <?> "ord(x) < 97"
    check p.debugParse("+") == $(ord('+'), 1, 0, 1)
    check p.debugParse("b") == $((unexpected: $ord('b'), expected: @[
        "ord(x) < 97"]), 0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "ord(x) < 97"]), 0, 0, 0)

  test "inClass":
    let
      vowel = inClass "aeiou"
      p = (satisfy vowel) <?> "vowel"
    check p.debugParse("foo") == $((unexpected: "\'f\'", expected: @["vowel"]),
        0, 0, 0)
    check p.debugParse("oo") == $('o', 1, 0, 1)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "vowel"]), 0, 0, 0)

    let
      halfAlphabet = inClass {'a'..'n', 'A'..'N'}
      q = (satisfy halfAlphabet) <?> "in half alphabet"
    check q.debugParse("foo") == $('f', 1, 0, 1)
    check q.debugParse("oo") == $((unexpected: "\'o\'", expected: @[
        "in half alphabet"]), 0, 0, 0)

  test "notInClass":
    let
      noVowel = notInClass "aeiou"
      p = (satisfy noVowel) <?> "no vowel"
    check p.debugParse("foo") == $('f', 1, 0, 1)
    check p.debugParse("oo") == $((unexpected: "\'o\'", expected: @[
        "no vowel"]), 0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "no vowel"]), 0, 0, 0)

    let
      notHalfAlphabet = notInClass {'a'..'n', 'A'..'N'}
      q = (satisfy notHalfAlphabet) <?> "not in half alphabet"
    check q.debugParse("foo") == $((unexpected: "\'f\'", expected: @[
        "not in half alphabet"]), 0, 0, 0)
    check q.debugParse("oo") == $('o', 1, 0, 1)

  test "anyChar":
    let p = anyChar
    check p.debugParse("foo") == $('f', 1, 0, 1)
    check p.debugParse("oo") == $('o', 1, 0, 1)
    check p.debugParse("f") == $('f', 1, 0, 1)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "any character"]), 0, 0, 0)

  test "ch":
    let p = ch('h')
    check p.debugParse("hello") == $('h', 1, 0, 1)
    check p.debugParse("ello") == $((unexpected: "\'e\'", expected: @["\'h\'"]),
        0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "\'h\'"]), 0, 0, 0)

  test "notChar":
    let p = notChar('e')
    check p.debugParse("hello") == $('h', 1, 0, 1)
    check p.debugParse("ello") == $((unexpected: "\'e\'", expected: @[
        "not \'e\'"]), 0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "not \'e\'"]), 0, 0, 0)

  test "peekCh":
    let p = peekCh
    check p.debugParse("foo") == $(some('f'), 0, 0, 0)
    check p.debugParse("oo") == $(some('o'), 0, 0, 0)
    check p.debugParse("f") == $(some('f'), 0, 0, 0)
    check p.debugParse("") == $(none(char), 0, 0, 0)

  test "peekChF":
    let p = peekChF
    check p.debugParse("foo") == $('f', 0, 0, 0)
    check p.debugParse("oo") == $('o', 0, 0, 0)
    check p.debugParse("f") == $('f', 0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "any character"]), 0, 0, 0)
