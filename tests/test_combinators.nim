import unittest

import microparsec

suite "basic combinators":
  test "attempt":
    let p = ch('h')
    check attempt(p).debugParse("hello") == p.debugParse("hello")
    check attempt(p).debugParse("ello") == p.debugParse("ello")
    check attempt(p).debugParse("") == p.debugParse("")

  test "<?>":
    let p = "if" <$ (ch('i') >> ch('f')) <?> "if statement"
    check p.debugParse("if 1 > 0") == $("if", 2, 0, 2)
    check p.debugParse("f 1 > 0") == $((unexpected: "\'f\'", expected: @["if statement"]), 0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @["if statement"]), 0, 0, 0)
