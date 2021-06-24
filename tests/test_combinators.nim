import unittest

import microparsec

let cases = ["hello", "ehllo", "ello", "hllo", "llo", ""]

suite "basic combinators":
  test "attempt":
    let p = ch 'h'
    for s in cases:
      check attempt(p).debugParse(s) == p.debugParse s

  test "<?>":
    let p = "if" <$ (ch('i') >> ch 'f') <?> "if statement"
    check p.debugParse("if 1 > 0") == $("if", 2, 0, 2)
    check p.debugParse("f 1 > 0") == $((unexpected: "\'f\'", expected: @[
        "if statement"]), 0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "if statement"]), 0, 0, 0)

  test "choice":
    let
      pc1 = choice([ch 'h'])
      pb1 = ch 'h'
    for s in cases:
      check pc1.debugParse(s) == pb1.debugParse s

    let
      pc2 = choice([ch 'h', ch 'e'])
      pb2 = ch('h') <|> ch 'e'
    for s in cases:
      check pc2.debugParse(s) == pb2.debugParse s

    let
      pc3 = choice([ch 'h', ch 'e', ch 'l'])
      pb3 = ch('h') <|> ch('e') <|> ch 'l'
    for s in cases:
      check pc3.debugParse(s) == pb3.debugParse s

  test "option":
    let p = option('c', ch 'a')
    check p.debugParse("aa") == $('a', 1, 0, 1)
    check p.debugParse("a") == $('a', 1, 0, 1)
    check p.debugParse("ba") == $('c', 0, 0, 0)
    check p.debugParse("b") == $('c', 0, 0, 0)
    check p.debugParse("") == $('c', 0, 0, 0)

  test "many1":
    let p = many1(ch 'h')
    # Both `seq[char]` and `string` work! Very useful! But structural matching
    # does not work (such as comparing tuples and one of the fields are
    # seq[char]/string! We need to specialize some functions to return string
    # instead of seq[char], and get rid of all "newSeq[char]" everywhere.
    check p.debugParse("hello") == $(@['h'], 1, 0, 1)
    check p.debugParse("hello") == $(@['h'], 1, 0, 1)
    check p.debugParse("hhello") == $(@['h', 'h'], 2, 0, 2)
    check p.debugParse("hhhello") == $(@['h', 'h', 'h'], 3, 0, 3)
    check p.debugParse("ello") == $((unexpected: "\'e\'", expected: @["\'h\'"]),
        0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "\'h\'"]), 0, 0, 0)

  test "sepBy":
    let p = sepBy(many1 digit, ch ',')
    check p.debugParse("1,2,3,4") == $(@[@['1'], @['2'], @['3'], @['4']], 7, 0, 7)
    check p.debugParse("11,22") == $(@[@['1', '1'], @['2', '2']], 5, 0, 5)

    check p.debugParse("11 ,22") == $(@[@['1', '1']], 2, 0, 2)
    check p.debugParse("11, 22") == $(@[@['1', '1']], 3, 0, 3)
    check p.debugParse("11,,22") == $(@[@['1', '1']], 3, 0, 3)
    check p.debugParse(",") == $(newSeq[seq[char]](), 0, 0, 0)
    check p.debugParse("") == $(newSeq[seq[char]](), 0, 0, 0)

    # The example below is from
    # <https://github.com/mrkkrp/megaparsec/issues/401#issue-572499736>.
    func foo[R, S, T](p: Parser[R], sep: Parser[S], q: Parser[T]): Parser[void] =
      sepBy(p, sep) >> optional(sep >> q)
    check foo(str "a", str " ", str "b").debugParse("a a b") == $(4, 0, 4)

  test "sepBy1":
    let
      inner = many1 digit
      p = sepBy1(inner, ch ',')
      q = sepBy(inner, ch ',')
    check p.debugParse("1,2,3,4") == q.debugParse("1,2,3,4")
    check p.debugParse("11,22") == q.debugParse("11,22")

    check p.debugParse("11 ,22") == q.debugParse("11 ,22")
    check p.debugParse("11, 22") == q.debugParse("11, 22")
    check p.debugParse("11,,22") == q.debugParse("11,,22")
    check p.debugParse(",") == $((unexpected: "\',\'", expected: @["digit"]), 0, 0, 0)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "digit"]), 0, 0, 0)

  test "manyTill":
    let p = manyTill(many(space) >> digit, ch '.')
    check p.debugParse("1 2  3\n4.") == $(@['1', '2', '3', '4'], 9, 1, 2)
    check p.debugParse("1 2  a\n4.") == $((unexpected: "\'a\'", expected: @[
        "digit"]), 5, 0, 5)
    check p.debugParse("1 2  3\n4") == $((unexpected: "end of input",
        expected: @["digit"]), 8, 1, 1)

    let simpleComment = str("<!--") >> manyTill(anyChar, str "-->")
    check simpleComment.debugParse("<!-- a -->") == $(@[' ', 'a', ' '], 10, 0, 10)
    check simpleComment.debugParse("<!-- a") == $((unexpected: "end of input",
        expected: @["any character"]), 6, 0, 6)
    check simpleComment.debugParse("a -->") == $((unexpected: "\'a\'",
        expected: @["\"<!--\""]), 0, 0, 0)
    check simpleComment.debugParse("-->") == $((unexpected: "\'-\'",
        expected: @["\"<!--\""]), 0, 0, 0)
    check simpleComment.debugParse("") == $((unexpected: "end of input",
        expected: @["\"<!--\""]), 0, 0, 0)

  test "skipMany":
    let p = skipMany(ch 'h')
    check p.debugParse("ello") == $(0, 0, 0)
    check p.debugParse("hello") == $(1, 0, 1)
    check p.debugParse("hhello") == $(2, 0, 2)
    check p.debugParse("hhhello") == $(3, 0, 3)
    check p.debugParse("") == $(0, 0, 0)

  test "skipMany1":
    let p = skipMany1(ch 'h')
    check p.debugParse("ello") == $((unexpected: "\'e\'", expected: @["\'h\'"]),
        0, 0, 0)
    check p.debugParse("hello") == $(1, 0, 1)
    check p.debugParse("hhello") == $(2, 0, 2)
    check p.debugParse("hhhello") == $(3, 0, 3)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "\'h\'"]), 0, 0, 0)

  test "count":
    let p = count(1, ch 'a')
    check p.debugParse("aa") == $(@['a'], 1, 0, 1)
    check p.debugParse("a") == $(@['a'], 1, 0, 1)
    check p.debugParse("") == $((unexpected: "end of input", expected: @[
        "\'a\'"]), 0, 0, 0)

    let q = count(2, ch 'a')
    check q.debugParse("aa") == $(@['a', 'a'], 2, 0, 2)
    check q.debugParse("ab") == $((unexpected: "\'b\'", expected: @["\'a\'"]),
        1, 0, 1)
    check q.debugParse("a") == $((unexpected: "end of input", expected: @[
        "\'a\'"]), 1, 0, 1)
    check q.debugParse("") == $((unexpected: "end of input", expected: @[
        "\'a\'"]), 0, 0, 0)
