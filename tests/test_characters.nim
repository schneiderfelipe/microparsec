import unittest

import microparsec

suite "single character parsers":
  test "inClass":
    let
      vowel = inClass "aeiou"
      p = (satisfy vowel) <?> "vowel"
    check p.debugParse("foo") == $(unexpected: "\'f\'", expected: @["vowel"])
    check p.debugParse("oo") == $('o', "o")
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "vowel"])

    let
      halfAlphabet = inClass {'a'..'n', 'A'..'N'}
      q = (satisfy halfAlphabet) <?> "in half alphabet"
    check q.debugParse("foo") == $('f', "oo")
    check q.debugParse("oo") == $(unexpected: "\'o\'", expected: @[
        "in half alphabet"])

  test "notInClass":
    let
      noVowel = notInClass "aeiou"
      p = (satisfy noVowel) <?> "no vowel"
    check p.debugParse("foo") == $('f', "oo")
    check p.debugParse("oo") == $(unexpected: "\'o\'", expected: @[
        "no vowel"])
    check p.debugParse("") == $(unexpected: "end of input", expected: @[
        "no vowel"])

    let
      notHalfAlphabet = notInClass {'a'..'n', 'A'..'N'}
      q = (satisfy notHalfAlphabet) <?> "not in half alphabet"
    check q.debugParse("foo") == $(unexpected: "\'f\'", expected: @[
        "not in half alphabet"])
    check q.debugParse("oo") == $('o', "o")

  test "oneOf":
    let
      vowel = oneOf "aeiou"
    check vowel.debugParse("foo") == $(unexpected: "\'f\'", expected: newSeq[string]())
    check vowel.debugParse("oo") == $('o', "o")
    check vowel.debugParse("") == $(unexpected: "end of input", expected: newSeq[string]())

  test "noneOf":
    let
      consonant = noneOf "aeiou"
    check consonant.debugParse("foo") == $('f', "oo")
    check consonant.debugParse("oo") == $(unexpected: "\'o\'", expected: newSeq[string]())
    check consonant.debugParse("") == $(unexpected: "end of input", expected: newSeq[string]())

  test "space":
    check space.debugParse(" ") == $(' ', "")
    check space.debugParse("\t") == $('\t', "")
    check space.debugParse("hello") == $(unexpected: "\'h\'", expected: @[
        "space"])
