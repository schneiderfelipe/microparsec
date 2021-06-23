import unittest

import microparsec

let cases = ["hello", "ehllo", "ello", "hllo", "llo", ""]

suite "basic combinators":
  test "attempt":
    let p = ch('h')
    for s in cases:
      check attempt(p).debugParse(s) == p.debugParse(s)

  test "<?>":
    let p = "if" <$ (ch('i') >> ch('f')) <?> "if statement"
    check p.debugParse("if 1 > 0") == $("if", 2, 0, 2)
    check p.debugParse("f 1 > 0") == $((unexpected: "\'f\'", expected: @[
        "if statement"]), 0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "if statement"]), 0, 0, 0)

  test "choice":
    let
      pc1 = choice([ch('h')])
      pb1 = ch('h')
    for s in cases:
      check pc1.debugParse(s) == pb1.debugParse(s)

    let
      pc2 = choice([ch('h'), ch('e')])
      pb2 = ch('h') <|> ch('e')
    for s in cases:
      check pc2.debugParse(s) == pb2.debugParse(s)

    let
      pc3 = choice([ch('h'), ch('e'), ch('l')])
      pb3 = ch('h') <|> ch('e') <|> ch('l')
    for s in cases:
      check pc3.debugParse(s) == pb3.debugParse(s)
