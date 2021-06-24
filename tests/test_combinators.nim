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
    check p.debugParse("if 1 > 0") == $("if", " 1 > 0")
    check p.debugParse("f 1 > 0") == $(unexpected: "\'f\'", expected: @[
        "if statement"])
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "if statement"])

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
    check p.debugParse("aa") == $('a', "a")
    check p.debugParse("a") == $('a', "")
    check p.debugParse("ba") == $('c', "ba")
    check p.debugParse("b") == $('c', "b")
    check p.debugParse("") == $('c', "")

  test "many1":
    let p = many1(ch 'h')
    # Both `seq[char]` and `string` work! Very useful! But structural matching
    # does not work (such as comparing tuples and one of the fields are
    # seq[char]/string! We need to specialize some functions to return string
    # instead of seq[char], and get rid of all "newSeq[char]" everywhere.
    check p.debugParse("hello") == $(@['h'], "ello")
    check p.debugParse("hhello") == $(@['h', 'h'], "ello")
    check p.debugParse("hhhello") == $(@['h', 'h', 'h'], "ello")
    check p.debugParse("ello") == $(unexpected: "\'e\'", expected: @["\'h\'"])
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "\'h\'"])

  test "sepBy":
    let p = sepBy(many1 digit, ch ',')
    check p.debugParse("1,2,3,4") == $(@[@['1'], @['2'], @['3'], @['4']], "")
    check p.debugParse("11,22") == $(@[@['1', '1'], @['2', '2']], "")

    check p.debugParse("11 ,22") == $(@[@['1', '1']], " ,22")
    check p.debugParse("11, 22") == $(@[@['1', '1']], " 22")
    check p.debugParse("11,,22") == $(@[@['1', '1']], ",22")
    check p.debugParse(",") == $(newSeq[seq[char]](), ",")
    check p.debugParse("") == $(newSeq[seq[char]](), "")

    # The example below is from
    # <https://github.com/mrkkrp/megaparsec/issues/401#issue-572499736>.
    func foo[R, S, T](p: Parser[R], sep: Parser[S], q: Parser[T]): Parser[void] =
      sepBy(p, sep) >> optional(sep >> q)
    check foo(str "a", str " ", str "b").debugParse("a a b") == "(\"b\")"

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
    check p.debugParse(",") == $(unexpected: "\',\'", expected: @["digit"])
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "digit"])

  test "manyTill":
    let p = manyTill(many(space) >> digit, ch '.')
    check p.debugParse("1 2  3\n4.") == $(@['1', '2', '3', '4'], "")
    check p.debugParse("1 2  a\n4.") == $(unexpected: "\'a\'", expected: @[
        "digit"])
    check p.debugParse("1 2  3\n4") == $(unexpected: "end of input",
        expected: @["digit"])

    let simpleComment = str("<!--") >> manyTill(anyChar, str "-->")
    check simpleComment.debugParse("<!-- a -->") == $(@[' ', 'a', ' '], "")
    check simpleComment.debugParse("<!-- a") == $(unexpected: "end of input",
        expected: @["any character"])
    check simpleComment.debugParse("a -->") == $(unexpected: "\'a\'",
        expected: @["\"<!--\""])
    check simpleComment.debugParse("-->") == $(unexpected: "\'-\'",
        expected: @["\"<!--\""])
    check simpleComment.debugParse("") == $(unexpected: "end of input",
        expected: @["\"<!--\""])

  test "skipMany":
    let p = skipMany(ch 'h')
    check p.debugParse("ello") == "(\"ello\")"
    check p.debugParse("hello") == "(\"ello\")"
    check p.debugParse("hhello") == "(\"ello\")"
    check p.debugParse("hhhello") == "(\"ello\")"

  test "skipMany1":
    let p = skipMany1(ch 'h')
    check p.debugParse("ello") == $(unexpected: "\'e\'", expected: @["\'h\'"])
    check p.debugParse("hello") == "(\"ello\")"
    check p.debugParse("hhello") == "(\"ello\")"
    check p.debugParse("hhhello") == "(\"ello\")"
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "\'h\'"])

  test "count":
    let p = count(1, ch 'a')
    check p.debugParse("aa") == $(@['a'], "a")
    check p.debugParse("a") == $(@['a'], "")
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "\'a\'"])

    let q = count(2, ch 'a')
    check q.debugParse("aa") == $(@['a', 'a'], "")
    check q.debugParse("ab") == $(unexpected: "\'b\'", expected: @["\'a\'"])
    check q.debugParse("a") == $(unexpected: "end of input", expected: @[
        "\'a\'"])
    check q.debugParse("") == $(unexpected: "end of input", expected: @[
        "\'a\'"])
