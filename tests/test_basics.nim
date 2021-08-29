# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest

import streams
import strutils

import microparsec

suite "basic character parsers":
  test "digit":
    check digit.debugParse("1hello") == $('1', "hello")
    check digit.debugParse("ello") == $(unexpected: "\'e\'", expected: @[
        "digit"])

  test "letter":
    check letter.debugParse("ello") == $('e', "llo")
    check letter.debugParse("1hello") == $(unexpected: "\'1\'", expected: @[
        "letter"])

  test "str":
    let p = str("hello")
    check p.debugParse("hello world") == $("hello", " world")
    check p.debugParse("1hello") == $(unexpected: "\'1\'", expected: @[
        "\"hello\""])
    check p.debugParse("ello") == $(unexpected: "\'e\'", expected: @[
        "\"hello\""])

  test "many":
    let p = many(ch('h'))
    # Both `seq[char]` and `string` work! Very useful! But structural matching
    # does not work (such as comparing tuples and one of the fields are
    # seq[char]/string! We need to specialize some functions to return string
    # instead of seq[char], and get rid of all "newSeq[char]" everywhere.
    check p.debugParse("hello") == $(@['h'], "ello")
    check p.debugParse("hhhello") == $(@['h', 'h', 'h'], "ello")
    check p.debugParse("ello") == $(newSeq[char](), "ello")

  test "pure":
    let p = pure 'x'
    check p.debugParse("hello") == $('x', "hello")
    check p.debugParse("") == $('x', "")

  test "liftA2":
    let p = liftA2((x: int, c: char) => (x, c), letter.map(c => ord(c) - ord(
        'a')), letter)
    check p.debugParse("hello") == $((7, 'e'), "llo")
    check p.debugParse("ello") == $((4, 'l'), "lo")
    check p.debugParse("1ello") == $(unexpected: "\'1\'", expected: @[
        "letter"])
    check p.debugParse("h2llo") == $(unexpected: "\'2\'", expected: @[
        "letter"])

    let q = liftA2((x: int, c: char) => (x, c), letter.map(c => ord(c) - ord(
        'a')), digit)
    check q.debugParse("hello") == $(unexpected: "\'e\'", expected: @[
        "digit"])
    check q.debugParse("1ello") == $(unexpected: "\'1\'", expected: @[
        "letter"])
    check q.debugParse("h2llo") == $((7, '2'), "llo")

  # Can't compare `ok`s due to a bug, see <https://github.com/arnetheduck/nim-result/issues/16>.
  # BUG: does not work in Nim 1.2.6.
  # test "eof":
  #   let p = eof
  #   check p.debugParse("") == $(0, 0, 0)
  #   check p.debugParse("hello") == $(unexpected: "\'h\'", expected: @["end of input"])

  test "between":
    let p = between(ch('{'), many(digit), ch('}'))
    check p.debugParse("{12}hello") == $(@['1', '2'], "hello")
    check p.debugParse("{}hello") == $(newSeq[char](), "hello")
    check p.debugParse("hello") == $(unexpected: "\'h\'", expected: @[
        "\'{\'"])
    check p.debugParse("{hello") == $(unexpected: "\'h\'", expected: @[
        "\'}\'"])
    check p.debugParse("{12hello") == $(unexpected: "\'h\'", expected: @[
        "\'}\'"])

    # Observe that the error message bypasses the possibility of more digits.
    # Think about the error messages as a set of tokens that would be required
    # to make the input valid.
    let q = between(ch('{'), many1(digit), ch('}'))
    check q.debugParse("{12}hello") == $(@['1', '2'], "hello")
    check q.debugParse("{}hello") == $(unexpected: "\'}\'", expected: @[
        "digit"])
    check q.debugParse("hello") == $(unexpected: "\'h\'", expected: @[
        "\'{\'"])
    check q.debugParse("{hello") == $(unexpected: "\'h\'", expected: @[
        "digit"])
    check q.debugParse("{12hello") == $(unexpected: "\'h\'", expected: @[
        "\'}\'"])

  test "optional":
    let p = optional(ch('h'))
    check p.debugParse("ello") == "(\"ello\")"
    check p.debugParse("hello") == "(\"ello\")"
    check p.debugParse("hhello") == "(\"hello\")"
    check p.debugParse("") == "(\"\")"

suite "parsing utilities":
  test "position state":
    let p = anyChar >> anyChar >> anyChar >> anyChar >> anyChar
    check p.debugParse("foo", withPosition = true) == $(
        unexpected: "end of input", expected: @["any character"])
    check p.debugParse("fooo", withPosition = true) == $(
        unexpected: "end of input", expected: @["any character"])
    check p.debugParse("foooo", withPosition = true) == $('o', "", 5, 0, 5)

    check p.debugParse("\nfoo", withPosition = true) == $(
        unexpected: "end of input", expected: @["any character"])
    check p.debugParse("f\noo", withPosition = true) == $(
        unexpected: "end of input", expected: @["any character"])
    check p.debugParse("fo\no", withPosition = true) == $(
        unexpected: "end of input", expected: @["any character"])
    check p.debugParse("foo\n", withPosition = true) == $(
        unexpected: "end of input", expected: @["any character"])

    check p.debugParse("\n\nfoo", withPosition = true) == $('o', "", 5, 2, 3)
    check p.debugParse("\nf\noo", withPosition = true) == $('o', "", 5, 2, 2)
    check p.debugParse("\nfo\no", withPosition = true) == $('o', "", 5, 2, 1)
    check p.debugParse("\nfoo\n", withPosition = true) == $('\n', "", 5, 1, 4)

    check p.debugParse("\nfooo", withPosition = true) == $('o', "", 5, 1, 4)
    check p.debugParse("f\nooo", withPosition = true) == $('o', "", 5, 1, 3)
    check p.debugParse("fo\noo", withPosition = true) == $('o', "", 5, 1, 2)
    check p.debugParse("foo\no", withPosition = true) == $('o', "", 5, 1, 1)
    check p.debugParse("fooo\n", withPosition = true) == $('\n', "", 5, 0,
        5) # Newline belongs to previous line

    check p.debugParse("\n\nfooo", withPosition = true) == $('o', "o", 6, 2, 3)
    check p.debugParse("\nf\nooo", withPosition = true) == $('o', "o", 6, 2, 2)
    check p.debugParse("\nfo\noo", withPosition = true) == $('o', "o", 6, 2, 1)
    check p.debugParse("\nfoo\no", withPosition = true) == $('\n', "o", 6, 1, 4)
    check p.debugParse("\nfooo\n", withPosition = true) == $('o', "\n", 6, 1, 4)

    check p.debugParse("\nfo\noo", withPosition = true) == $('o', "o", 6, 2, 1)
    check p.debugParse("f\no\noo", withPosition = true) == $('o', "o", 6, 2, 1)
    check p.debugParse("fo\n\noo", withPosition = true) == $('o', "o", 6, 2, 1)
    check p.debugParse("foo\n\no", withPosition = true) == $('\n', "o", 6, 1, 0)
    check p.debugParse("foo\no\n", withPosition = true) == $('o', "\n", 6, 1, 1)

    check p.debugParse("", withPosition = true) == $(unexpected: "end of input",
        expected: @["any character"])

  test "error messages":
    let p = anyChar >> anyChar >> anyChar >> anyChar >> anyChar
    check $p.parse("f\noo") == """Failed reading: satisfy

1:2:(4):
  |
1 | oo
  |   ^
unexpected end of input
expecting any character"""

    let q = anyChar >> ch('a')
    check $q.parse("hello") == """Failed reading: satisfy

0:1:(1):
  |
0 | hello
  |  ^
unexpected 'e'
expecting 'a'"""

    let r = (str("hello") <|> str("hey")) >> many(ch(' ')) >> (str("world") <|>
        str("joe"))
    check $r.parse("hello  john") == """Failed reading: satisfy

0:9:(9):
  |
0 | hello  john
  |          ^
unexpected 'h'
expecting "world" or "joe""""

suite "parser combinators":
  test "<|>":
    let p = ch('h') <|> ch('e')
    check p.debugParse("hello") == $('h', "ello")
    check p.debugParse("ehllo") == $('e', "hllo")
    check p.debugParse("ello") == $('e', "llo")
    check p.debugParse("hllo") == $('h', "llo")
    check p.debugParse("llo") == $(unexpected: "\'l\'", expected: @["\'h\'",
        "\'e\'"])
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "\'h\'", "\'e\'"])

  test ">>":
    let p = ch('h') >> ch('e')
    check p.debugParse("hello") == $('e', "llo")
    check p.debugParse("ello") == $(unexpected: "\'e\'", expected: @["\'h\'"])
    check p.debugParse("hllo") == $(unexpected: "\'l\'", expected: @["\'e\'"])
    check p.debugParse("llo") == $(unexpected: "\'l\'", expected: @["\'h\'"])
    check p.debugParse("heklo") == $('e', "klo")
    check p.debugParse("helko") == $('e', "lko")
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "\'h\'"])

    check (ch('h') >> ch('e')).debugParse("hello") == (ch('h') *> ch(
        'e')).debugParse("hello")
    check (ch('h') >> ch('e')).debugParse("ello") == (ch('h') *> ch(
        'e')).debugParse("ello")
    check (ch('h') >> ch('e')).debugParse("hllo") == (ch('h') *> ch(
        'e')).debugParse("hllo")
    check (ch('h') >> ch('e')).debugParse("llo") == (ch('h') *> ch(
        'e')).debugParse("llo")
    check (ch('h') >> ch('e')).debugParse("heklo") == (ch('h') *> ch(
        'e')).debugParse("heklo")
    check (ch('h') >> ch('e')).debugParse("helko") == (ch('h') *> ch(
        'e')).debugParse("helko")
    check (ch('h') >> ch('e')).debugParse("") == (ch('h') *> ch(
        'e')).debugParse("")

  test "*>":
    let p = (ch('h') >> ch('e')) *> ch('l') >> ch('l')
    check p.debugParse("hello") == $('l', "o")
    check p.debugParse("ello") == $(unexpected: "\'e\'", expected: @["\'h\'"])
    check p.debugParse("hllo") == $(unexpected: "\'l\'", expected: @["\'e\'"])
    check p.debugParse("llo") == $(unexpected: "\'l\'", expected: @["\'h\'"])
    check p.debugParse("heklo") == $(unexpected: "\'k\'", expected: @[
        "\'l\'"])
    check p.debugParse("helko") == $(unexpected: "\'k\'", expected: @[
        "\'l\'"])
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "\'h\'"])

  test "<*":
    let p = ch('a') <* ch('-')
    check p.debugParse("a-") == $('a', "")
    check p.debugParse("aa-") == $(unexpected: "\'a\'", expected: @["\'-\'"])
    check p.debugParse("b-") == $(unexpected: "\'b\'", expected: @["\'a\'"])
    check p.debugParse("-") == $(unexpected: "\'-\'", expected: @["\'a\'"])
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "\'a\'"])

  test "<$":
    let p = true <$ (ch('h') >> ch('e'))
    check p.debugParse("hello") == $(true, "llo")
    check p.debugParse("ello") == $(unexpected: "\'e\'", expected: @["\'h\'"])
    check p.debugParse("hllo") == $(unexpected: "\'l\'", expected: @["\'e\'"])
    check p.debugParse("llo") == $(unexpected: "\'l\'", expected: @["\'h\'"])
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "\'h\'"])
