import unittest

import microparsec

suite "single character parsers":
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
