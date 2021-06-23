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
    check digit.debugParse("1hello") == $('1', 1, 0, 1)
    check digit.debugParse("ello") == $((unexpected: "\'e\'", expected: @[
        "digit"]), 0, 0, 0)
    check digit.debugParse("") == $((unexpected: "end of input", expected: @[
        "digit"]), 0, 0, 0)

  test "letter":
    check letter.debugParse("ello") == $('e', 1, 0, 1)
    check letter.debugParse("1hello") == $((unexpected: "\'1\'", expected: @[
        "letter"]), 0, 0, 0)
    check letter.debugParse("") == $((unexpected: "end of input", expected: @[
        "letter"]), 0, 0, 0)

  test "space":
    check space.debugParse(" ") == $(' ', 1, 0, 1)
    check space.debugParse("\t") == $('\t', 1, 0, 1)
    check space.debugParse("\n") == $('\n', 1, 0, 1)
    check space.debugParse("hello") == $((unexpected: "\'h\'", expected: @[
        "space"]), 0, 0, 0)
    check space.debugParse("") == $((unexpected: "end of input", expected: @[
        "space"]), 0, 0, 0)

  test "str":
    let p = str("hello")
    check p.debugParse("hello") == $("hello", 5, 0, 5)
    check p.debugParse("hello world") == $("hello", 5, 0, 5)
    check p.debugParse("1hello") == $((unexpected: "\'1\'", expected: @[
        "\"hello\""]), 0, 0, 0)
    check p.debugParse("ello") == $((unexpected: "\'e\'", expected: @[
        "\"hello\""]), 0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "\"hello\""]), 0, 0, 0)

  test "many":
    let p = many(ch('h'))
    # Both `seq[char]` and `string` work! Very useful! But structural matching
    # does not work (such as comparing tuples and one of the fields are
    # seq[char]/string! We need to specialize some functions to return string
    # instead of seq[char], and get rid of all "newSeq[char]" everywhere.
    check p.debugParse("hello") == $(@['h'], 1, 0, 1)
    check p.debugParse("hello") == $(@['h'], 1, 0, 1)
    check p.debugParse("hhello") == $(@['h', 'h'], 2, 0, 2)
    check p.debugParse("hhhello") == $(@['h', 'h', 'h'], 3, 0, 3)
    check p.debugParse("ello") == $(newSeq[char](), 0, 0, 0)
    check p.debugParse("") == $(newSeq[char](), 0, 0, 0)

  test "pure":
    let p = pure 'x'
    check p.debugParse("hello") == $('x', 0, 0, 0)
    check p.debugParse("ello") == $('x', 0, 0, 0)
    check p.debugParse("") == $('x', 0, 0, 0)

  test "liftA2":
    let p = liftA2((x: int, c: char) => (x, c), letter.map(c => ord(c) - ord(
        'a')), letter)
    check p.debugParse("hello") == $((7, 'e'), 2, 0, 2)
    check p.debugParse("ello") == $((4, 'l'), 2, 0, 2)
    check p.debugParse("1ello") == $((unexpected: "\'1\'", expected: @[
        "letter"]), 0, 0, 0)
    check p.debugParse("h2llo") == $((unexpected: "\'2\'", expected: @[
        "letter"]), 1, 0, 1)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "letter"]), 0, 0, 0)

    let q = liftA2((x: int, c: char) => (x, c), letter.map(c => ord(c) - ord(
        'a')), digit)
    check q.debugParse("hello") == $((unexpected: "\'e\'", expected: @[
        "digit"]), 1, 0, 1)
    check q.debugParse("ello") == $((unexpected: "\'l\'", expected: @["digit"]),
        1, 0, 1)
    check q.debugParse("1ello") == $((unexpected: "\'1\'", expected: @[
        "letter"]), 0, 0, 0)
    check q.debugParse("h2llo") == $((7, '2'), 2, 0, 2)
    check q.debugParse("") == $((unexpected: "end of input", expected: @[
        "letter"]), 0, 0, 0)

  # Can't compare `ok`s due to a bug, see <https://github.com/arnetheduck/nim-result/issues/16>.
  # BUG: does not work in Nim 1.2.6.
  # test "eof":
  #   let p = eof
  #   check p.debugParse("") == $(0, 0, 0)
  #   check p.debugParse("hello") == $((unexpected: "\'h\'", expected: @["end of input"]), 0, 0, 0)

  test "between":
    let p = between(ch('{'), many(digit), ch('}'))
    check p.debugParse("{12}hello") == $(@['1', '2'], 4, 0, 4)
    check p.debugParse("{}hello") == $(newSeq[char](), 2, 0, 2)
    check p.debugParse("hello") == $((unexpected: "\'h\'", expected: @[
        "\'{\'"]), 0, 0, 0)
    check p.debugParse("{hello") == $((unexpected: "\'h\'", expected: @[
        "\'}\'"]), 1, 0, 1)
    check p.debugParse("{1hello") == $((unexpected: "\'h\'", expected: @[
        "\'}\'"]), 2, 0, 2)
    check p.debugParse("{12hello") == $((unexpected: "\'h\'", expected: @[
        "\'}\'"]), 3, 0, 3)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "\'{\'"]), 0, 0, 0)

    # Observe that the error message bypasses the possibility of more digits.
    # Think about the error messages as a set of tokens that would be required
    # to make the input valid.
    let q = between(ch('{'), many1(digit), ch('}'))
    check q.debugParse("{12}hello") == $(@['1', '2'], 4, 0, 4)
    check q.debugParse("{}hello") == $((unexpected: "\'}\'", expected: @[
        "digit"]), 1, 0, 1)
    check q.debugParse("hello") == $((unexpected: "\'h\'", expected: @[
        "\'{\'"]), 0, 0, 0)
    check q.debugParse("{hello") == $((unexpected: "\'h\'", expected: @[
        "digit"]), 1, 0, 1)
    check q.debugParse("{1hello") == $((unexpected: "\'h\'", expected: @[
        "\'}\'"]), 2, 0, 2)
    check q.debugParse("{12hello") == $((unexpected: "\'h\'", expected: @[
        "\'}\'"]), 3, 0, 3)
    check q.debugParse("") == $((unexpected: "end of input", expected: @[
        "\'{\'"]), 0, 0, 0)

  test "sepBy":
    let p = sepBy(many1(digit), ch(','))
    check p.debugParse("1,2,3,4") == $(@[@['1'], @['2'], @['3'], @['4']], 7, 0, 7)
    check p.debugParse("11,22") == $(@[@['1', '1'], @['2', '2']], 5, 0, 5)
    # Observe how forgiving is that. Also observe how greedy that is.
    check p.debugParse("11 ,22") == $(@[@['1', '1']], 2, 0, 2)
    check p.debugParse("11, 22") == $(@[@['1', '1']], 3, 0, 3)
    check p.debugParse("11,,22") == $(@[@['1', '1']], 3, 0, 3)
    check p.debugParse(",") == $(newSeq[seq[char]](), 0, 0, 0)
    check p.debugParse("") == $(newSeq[seq[char]](), 0, 0, 0)
    # check p.debugParse("1,2,3,4") == $(@["\'1\'", "\'2\'", "\'3\'", "\'4\'"], 7, 0, 7)
    # check p.debugParse("") == $((unexpected: "end of input", expected: @["\'{\'"]), 0, 0, 0)


    # If you think sepBy should not be eager, think again: it should. See
    # <https://github.com/mrkkrp/megaparsec/issues/401#issue-572499736>,
    # whose example is reproduced below.
    func foo[R, S, T](p: Parser[R], sep: Parser[S], q: Parser[T]): Parser[void] =
      sepBy(p, sep) >> optional(sep >> q)
    check foo(str("a"), str(" "), str("b")).debugParse("a a b") == $(4, 0, 4)

  test "sepBy1":
    let p = sepBy1(many1(digit), ch(','))
    check p.debugParse("1,2,3,4") == $(@[@['1'], @['2'], @['3'], @['4']], 7, 0, 7)
    check p.debugParse("11,22") == $(@[@['1', '1'], @['2', '2']], 5, 0, 5)
    # Observe how forgiving is that. Also observe how greedy that is.
    check p.debugParse("11 ,22") == $(@[@['1', '1']], 2, 0, 2)
    check p.debugParse("11, 22") == $(@[@['1', '1']], 3, 0, 3)
    check p.debugParse("11,,22") == $(@[@['1', '1']], 3, 0, 3)
    check p.debugParse(",") == $((unexpected: "\',\'", expected: @["digit"]), 0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "digit"]), 0, 0, 0)
    # check p.debugParse("1,2,3,4") == $(@["\'1\'", "\'2\'", "\'3\'", "\'4\'"], 7, 0, 7)
    # check p.debugParse("") == $((unexpected: "end of input", expected: @["\'{\'"]), 0, 0, 0)

  test "optional":
    let p = optional(ch('h'))
    check p.debugParse("ello") == $(0, 0, 0)
    check p.debugParse("hello") == $(1, 0, 1)
    check p.debugParse("hhello") == $(1, 0, 1)
    check p.debugParse("") == $(0, 0, 0)

suite "parsing utilities":
  test "position state":
    let p = anyChar >> anyChar >> anyChar >> anyChar >> anyChar
    check p.debugParse("foo") == $((unexpected: "end of input", expected: @[
        "any character"]), 3, 0, 3)
    check p.debugParse("fooo") == $((unexpected: "end of input", expected: @[
        "any character"]), 4, 0, 4)
    check p.debugParse("foooo") == $('o', 5, 0, 5)

    check p.debugParse("\nfoo") == $((unexpected: "end of input", expected: @[
        "any character"]), 4, 1, 3)
    check p.debugParse("f\noo") == $((unexpected: "end of input", expected: @[
        "any character"]), 4, 1, 2)
    check p.debugParse("fo\no") == $((unexpected: "end of input", expected: @[
        "any character"]), 4, 1, 1)
    check p.debugParse("foo\n") == $((unexpected: "end of input", expected: @[
        "any character"]), 4, 0, 4)

    check p.debugParse("\n\nfoo") == $('o', 5, 2, 3)
    check p.debugParse("\nf\noo") == $('o', 5, 2, 2)
    check p.debugParse("\nfo\no") == $('o', 5, 2, 1)
    check p.debugParse("\nfoo\n") == $('\n', 5, 1, 4)

    check p.debugParse("\nfooo") == $('o', 5, 1, 4)
    check p.debugParse("f\nooo") == $('o', 5, 1, 3)
    check p.debugParse("fo\noo") == $('o', 5, 1, 2)
    check p.debugParse("foo\no") == $('o', 5, 1, 1)
    check p.debugParse("fooo\n") == $('\n', 5, 0, 5) # Newline belongs to previous line

    check p.debugParse("\n\nfooo") == $('o', 5, 2, 3)
    check p.debugParse("\nf\nooo") == $('o', 5, 2, 2)
    check p.debugParse("\nfo\noo") == $('o', 5, 2, 1)
    check p.debugParse("\nfoo\no") == $('\n', 5, 1, 4)
    check p.debugParse("\nfooo\n") == $('o', 5, 1, 4)

    check p.debugParse("\nfo\noo") == $('o', 5, 2, 1)
    check p.debugParse("f\no\noo") == $('o', 5, 2, 1)
    check p.debugParse("fo\n\noo") == $('o', 5, 2, 1)
    check p.debugParse("foo\n\no") == $('\n', 5, 1, 0)
    check p.debugParse("foo\no\n") == $('o', 5, 1, 1)

    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "any character"]), 0, 0, 0)

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
    check p.debugParse("hello") == $('h', 1, 0, 1)
    check p.debugParse("ehllo") == $('e', 1, 0, 1)
    check p.debugParse("ello") == $('e', 1, 0, 1)
    check p.debugParse("hllo") == $('h', 1, 0, 1)
    check p.debugParse("llo") == $((unexpected: "\'l\'", expected: @["\'h\'",
        "\'e\'"]), 0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "\'h\'", "\'e\'"]), 0, 0, 0)

  test ">>":
    let p = ch('h') >> ch('e')
    check p.debugParse("hello") == $('e', 2, 0, 2)
    check p.debugParse("ello") == $((unexpected: "\'e\'", expected: @["\'h\'"]),
        0, 0, 0)
    check p.debugParse("hllo") == $((unexpected: "\'l\'", expected: @["\'e\'"]),
        1, 0, 1)
    check p.debugParse("llo") == $((unexpected: "\'l\'", expected: @["\'h\'"]),
        0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "\'h\'"]), 0, 0, 0)

  test "*>":
    let p = (ch('h') >> ch('e')) *> ch('l') >> ch('l')
    check p.debugParse("hello") == $('l', 4, 0, 4)
    check p.debugParse("llo") == $('l', 2, 0, 2)
    check p.debugParse("heklo") == $((unexpected: "\'k\'", expected: @[
        "\'l\'"]), 2, 0, 2)
    # check p.debugParse("ello") == $((unexpected: "\'e\'", expected: @["\'h\'", "\'l\'"]), 0, 0, 0)
    # check p.debugParse("hllo") == $((unexpected: "\'l\'", expected: @["\'e\'"]), 1, 0, 1)
    # check p.debugParse("") == $((unexpected: "end of input", expected: @["\'h\'", "\'l\'"]), 0, 0, 0)

  test "<*":
    let p = ch('a') <* ch('-')
    check p.debugParse("a-") == $('a', 2, 0, 2)
    check p.debugParse("aa-") == $('a', 1, 0, 1)
    check p.debugParse("b-") == $((unexpected: "\'b\'", expected: @["\'a\'"]),
        0, 0, 0)
    check p.debugParse("-") == $((unexpected: "\'-\'", expected: @["\'a\'"]), 0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "\'a\'"]), 0, 0, 0)

  test "<$":
    let p = true <$ (ch('h') >> ch('e'))
    check p.debugParse("hello") == $(true, 2, 0, 2)
    check p.debugParse("ello") == $((unexpected: "\'e\'", expected: @["\'h\'"]),
        0, 0, 0)
    check p.debugParse("hllo") == $((unexpected: "\'l\'", expected: @["\'e\'"]),
        1, 0, 1)
    check p.debugParse("llo") == $((unexpected: "\'l\'", expected: @["\'h\'"]),
        0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "\'h\'"]), 0, 0, 0)
