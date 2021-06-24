import unittest

import microparsec

suite "common use cases":
  test "identifier":
    # A `Parser` that consumes a common identifier, made of letters, digits and
    # underscores (`'_'`). Close to
    # <https://hackage.haskell.org/package/parser-combinators-1.2.1/docs/Control-Monad-Combinators.html#v:many>.
    let identifier = many1(letter <|> digit <|> ch('_'))
    # Both `seq[char]` and `string` work! Very useful! But structural matching
    # does not work (such as comparing tuples and one of the fields are
    # seq[char]/string! We need to specialize some functions to return string
    # instead of seq[char], and get rid of all "newSeq[char]" everywhere.
    check identifier.debugParse("hello") == $(@['h', 'e', 'l', 'l', 'o'], "")
    check identifier.debugParse("hello") == $(@['h', 'e', 'l', 'l', 'o'], "")
    check identifier.debugParse("hello world") == $(@['h', 'e', 'l', 'l', 'o'],
        " world")
    check identifier.debugParse("123hello_ world") == $(@['1', '2', '3', 'h',
        'e', 'l', 'l', 'o', '_'], " world")
    check identifier.debugParse("*123hello_ world") == $(unexpected: "\'*\'",
        expected: @["letter", "digit", "\'_\'"])
    check identifier.debugParse("") == $(unexpected: "end of input",
        expected: @["letter", "digit", "\'_\'"])

    check $identifier.parse("*123hello_ world") == """Failed reading: satisfy

0:0:(0):
  |
0 | *123hello_ world
  | ^
unexpected '*'
expecting letter, digit, or '_'"""
