# Microparsec ⭐

Microparsec is a fast
[parser combinator library](https://en.wikipedia.org/wiki/Parser_combinator)
with excellent error messages.

```nim
import microparsec
let p = str("hello") >> many(ch ' ') >> str("world!")
p.parse("hello     world?")
```
```
0:15:(15):
  |
0 | hello     world?
  |                ^
unexpected '?'
expecting "world!"
```

Microparsec is a pure
[Nim](https://nim-lang.org/)
adaptation of
[Parsec](https://github.com/haskell/parsec),
the popular
[monadic](https://en.wikipedia.org/wiki/Monad_(functional_programming))
parser combinator library for
[Haskell](https://www.haskell.org/) by
[Daan Leijen](https://www.microsoft.com/en-us/research/people/daan/),
Further inspiration was taken from
[Attoparsec](https://github.com/haskell/attoparsec)
and
[Megaparsec](https://github.com/mrkkrp/megaparsec).

## References

- Leijen, Daan & Meijer, Erik. (2001). **Parsec: Direct Style Monadic Parser Combinators For The Real World**
  ([link](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parsec-paper-letter.pdf)).
- Holden, Daniel. (2014). **You could have invented Parser Combinators**
  ([link](http://theorangeduck.com/page/you-could-have-invented-parser-combinators)).

## Similar projects

- [Attoparsec](https://github.com/haskell/attoparsec) (Haskell)
- [Megaparsec](https://github.com/mrkkrp/megaparsec) (Haskell)
- [Parsec](https://github.com/haskell/parsec) (Haskell)
<!-- - [FParsec](http://www.quanttec.com/fparsec/) (F#) -->
