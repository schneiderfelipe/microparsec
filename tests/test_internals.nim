import unittest

import microparsec

suite "single character parsers":
  test "satisfy":
    let p = (satisfy do (c: char) -> auto:
      c in {'+', '-', '*', '/'}) <?> "operation"
    check p.debugParse("+") == $('+', 1, 0, 1)
    check p.debugParse("1") == $((unexpected: "\'1\'", expected: @["operation"]), 0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @["operation"]), 0, 0, 0)

  test "skip":
    let p = (skip do (c: char) -> auto:
      c in {'+', '-', '*', '/'}) <?> "skippable operation"
    check p.debugParse("+") == $(1, 0, 1)
    check p.debugParse("1") == $((unexpected: "\'1\'", expected: @["skippable operation"]), 0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @["skippable operation"]), 0, 0, 0)

  test "satisfyWith":
    let p = (satisfyWith(c => ord(c)) do (x: auto) -> auto:
      x < 97) <?> "ord(x) < 97"
    check p.debugParse("+") == $(ord('+'), 1, 0, 1)
    check p.debugParse("b") == $((unexpected: $ord('b'), expected: @["ord(x) < 97"]), 0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @["ord(x) < 97"]), 0, 0, 0)

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
    check p.debugParse("ello") == $((unexpected: "\'e\'", expected: @["not \'e\'"]),
        0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "not \'e\'"]), 0, 0, 0)
