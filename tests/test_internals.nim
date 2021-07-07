import unittest

import microparsec

suite "single character parsers":
  test "satisfy":
    let p = (satisfy do (c: char) -> auto:
      c in {'+', '-', '*', '/'}) <?> "operation"
    check p.debugParse("+") == $('+', "")
    check p.debugParse("1") == $(unexpected: "\'1\'", expected: @[
        "operation"])
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "operation"])

  test "skip":
    let p = (skip do (c: char) -> auto:
      c in {'+', '-', '*', '/'}) <?> "skippable operation"
    check p.debugParse("+") == "(\"\")"
    check p.debugParse("1") == $(unexpected: "\'1\'", expected: @[
        "skippable operation"])
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "skippable operation"])

  test "satisfyWith":
    let p = (satisfyWith(c => ord(c)) do (x: auto) -> auto:
      x < 97) <?> "ord(x) < 97"
    check p.debugParse("+") == $(ord('+'), "")
    check p.debugParse("b") == $(unexpected: $ord('b'), expected: @[
        "ord(x) < 97"])
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "ord(x) < 97"])

  test "anyChar":
    let p = anyChar
    check p.debugParse("foo") == $('f', "oo")
    check p.debugParse("oo") == $('o', "o")
    check p.debugParse("f") == $('f', "")
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "any character"])

  test "ch":
    let p = ch('h')
    check p.debugParse("hello") == $('h', "ello")
    check p.debugParse("ello") == $(unexpected: "\'e\'", expected: @["\'h\'"])
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "\'h\'"])

  test "notChar":
    let p = notChar('e')
    check p.debugParse("hello") == $('h', "ello")
    check p.debugParse("ello") == $(unexpected: "\'e\'", expected: @[
        "not \'e\'"])
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "not \'e\'"])

  test "peekCh":
    let p = peekCh
    check p.debugParse("foo") == $(some('f'), "foo")
    check p.debugParse("oo") == $(some('o'), "oo")
    check p.debugParse("f") == $(some('f'), "f")
    check p.debugParse("") == $(none(char), "")

  test "peekChF":
    let p = peekChF
    check p.debugParse("foo") == $('f', "foo")
    check p.debugParse("oo") == $('o', "oo")
    check p.debugParse("f") == $('f', "f")
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "any character"])

  test "match":
    let p = match(str("hello") >> many(space) >> str("world").map(x => true))
    check p.debugParse("hello world") == $(("hello world", true), "")
    check p.debugParse("hello   world") == $(("hello   world", true), "")
    check p.debugParse("hello  joe") == $(unexpected: "\'j\'", expected: @[
        "\"world\""])
    check p.debugParse("foo") == $(unexpected: "\'f\'", expected: @[
        "\"hello\""])
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "\"hello\""])
