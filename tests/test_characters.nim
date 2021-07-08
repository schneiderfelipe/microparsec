import unittest

import microparsec

suite "single character parsers":
  test "inClass":
    let
      vowel = inClass "aeiou"
      p = satisfy(vowel) <?> "vowel"
    check p.debugParse("foo") == $(unexpected: "\'f\'", expected: @["vowel"])
    check p.debugParse("oo") == $('o', "o")
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "vowel"])

    let
      halfAlphabet = inClass {'a'..'n', 'A'..'N'}
      q = satisfy(halfAlphabet) <?> "in half alphabet"
    check q.debugParse("foo") == $('f', "oo")
    check q.debugParse("oo") == $(unexpected: "\'o\'", expected: @[
        "in half alphabet"])

  test "notInClass":
    let
      noVowel = notInClass "aeiou"
      p = satisfy(noVowel) <?> "no vowel"
    check p.debugParse("foo") == $('f', "oo")
    check p.debugParse("oo") == $(unexpected: "\'o\'", expected: @[
        "no vowel"])
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "no vowel"])

    let
      notHalfAlphabet = notInClass {'a'..'n', 'A'..'N'}
      q = satisfy(notHalfAlphabet) <?> "not in half alphabet"
    check q.debugParse("foo") == $(unexpected: "\'f\'", expected: @[
        "not in half alphabet"])
    check q.debugParse("oo") == $('o', "o")

  test "oneOf":
    let
      vowel = oneOf "aeiou"
    check vowel.debugParse("foo") == $(unexpected: "\'f\'", expected: newSeq[
        string]())
    check vowel.debugParse("oo") == $('o', "o")
    check vowel.debugParse("") == $(unexpected: "end of input",
        expected: newSeq[string]())

  test "noneOf":
    let
      consonant = noneOf "aeiou"
    check consonant.debugParse("foo") == $('f', "oo")
    check consonant.debugParse("oo") == $(unexpected: "\'o\'", expected: newSeq[
        string]())
    check consonant.debugParse("") == $(unexpected: "end of input",
        expected: newSeq[string]())

  test "space":
    check space.debugParse(" ") == $(' ', "")
    check space.debugParse("\t") == $('\t', "")
    check space.debugParse("hello") == $(unexpected: "\'h\'", expected: @[
        "space"])

  test "spaces":
    check spaces.debugParse(" ") == "(\"\")"
    check spaces.debugParse("     ") == "(\"\")"
    check spaces.debugParse("\t") == "(\"\")"
    check spaces.debugParse(" \t\n \n") == "(\"\")"
    check spaces.debugParse("hello") == "(\"hello\")"
    check spaces.debugParse("   \nhello") == "(\"hello\")"

  test "newline":
    check newline.debugParse("\n") == $('\n', "")
    check newline.debugParse("\r") == $(unexpected: "\'\\c\'", expected: @["lf new-line"])
    check newline.debugParse("\r\n") == $(unexpected: "\'\\c\'", expected: @["lf new-line"])
    check newline.debugParse("\n\r") == $('\n', "\c")
    check newline.debugParse("\nhello") == $('\n', "hello")
    check newline.debugParse(" ") == $(unexpected: "\' \'", expected: @["lf new-line"])
    check newline.debugParse("") == $(unexpected: "end of input", expected: @["lf new-line"])

  test "crlf":
    check crlf.debugParse("\n") == $(unexpected: "\'\\n\'", expected: @["crlf new-line"])
    check crlf.debugParse("\r") == $(unexpected: "end of input", expected: @["crlf new-line"])
    check crlf.debugParse("\r\n") == $('\n', "")
    check crlf.debugParse("\n\r") == $(unexpected: "\'\\n\'", expected: @["crlf new-line"])
    check crlf.debugParse("\nhello") == $(unexpected: "\'\\n\'", expected: @["crlf new-line"])
    check crlf.debugParse(" ") == $(unexpected: "\' \'", expected: @["crlf new-line"])
    check crlf.debugParse("") == $(unexpected: "end of input", expected: @["crlf new-line"])

  test "endOfLine":
    check endOfLine.debugParse("\n") == $('\n', "")
    check endOfLine.debugParse("\r") == $(unexpected: "end of input",
        expected: @["new-line"])
    check endOfLine.debugParse("\r\n") == $('\n', "")
    check endOfLine.debugParse("\n\r") == $('\n', "\c")
    check endOfLine.debugParse("\nhello") == $('\n', "hello")
    check endOfLine.debugParse(" ") == $(unexpected: "\' \'", expected: @["new-line"])
    check endOfLine.debugParse("") == $(unexpected: "end of input", expected: @["new-line"])
