<a href="http://schneiderfelipe.xyz/microparsec/">
  <img alt="Documentation" src="https://img.shields.io/badge/docs-available-brightgreen"/>
</a>

# Microparsec ⭐

[WIP] Microparsec is a fast
[parser combinator library](https://en.wikipedia.org/wiki/Parser_combinator)
with excellent error messages.

```nim
import microparsec
let p = between(
  ch('{'),
  str("hello") >> many(space) >> str("world!"),
  ch('}')
)
echo p.parse("{hello\n world?}")
```

```
Failed reading: satisfy

1:6:(13):
  |
1 |  world?}
  |       ^
unexpected '?'
expecting "world!"
```

Microparsec is a pure
[Nim](https://nim-lang.org/)
adaptation of
[Parsec](https://github.com/haskell/parsec),
the popular
[monadic](<https://en.wikipedia.org/wiki/Monad_(functional_programming)>)
parser combinator library for
[Haskell](https://www.haskell.org/) by
[Daan Leijen](https://www.microsoft.com/en-us/research/people/daan/),
Further inspiration was taken from
[Attoparsec](https://github.com/haskell/attoparsec)
and
[Megaparsec](https://github.com/mrkkrp/megaparsec).

## Installation

Microparsec supports Nim 1.2.6+ and can be installed using [Nimble](https://github.com/nim-lang/Nimble):

    $ nimble install microparsec

## Some references

-   Leijen, Daan & Meijer, Erik. (2001).
    [**Parsec: Direct Style Monadic Parser Combinators For The Real World**](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parsec-paper-letter.pdf).
-   Holden, Daniel. (2014).
    [**You could have invented Parser Combinators**](http://theorangeduck.com/page/you-could-have-invented-parser-combinators).

## Inspiring projects

-   [Attoparsec](https://github.com/haskell/attoparsec) (Haskell)
-   [Megaparsec](https://github.com/mrkkrp/megaparsec) (Haskell)
-   [Parsec](https://github.com/haskell/parsec) (Haskell)
<!-- - [FParsec](http://www.quanttec.com/fparsec/) (F#) -->
