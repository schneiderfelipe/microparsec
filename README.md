# Microparsec

Microparsec is a monadic parser combinator library for Nim.

```nim
import src/microparsec

let p = str("hello") >> many(ch(' ')) >> str("world!")
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

It is a feature-rich package built for **speed**, **flexibility**, **portability**, and **quality of parse errors**.

<!-- Microparsec's features include:

- support for context‐sensitive, infinite look‐ahead grammars
- automatically generated, highly readable error messages
- Unicode support
- efficient support for very large files
- an embeddable, runtime‐configurable operator‐precedence parser component
- a simple, efficient and easily extensible API
- an implementation thoroughly optimized for performance
- comprehensive documentation
- a permissive open source license

Microparsec is a Nim adaptation of Parsec, the popular parser combinator library for Haskell by Daan Leijen.
While the implementations of Parsec and Microparsec are completely different, they share a similar top‐level API. -->

## Similar projects

- [Megaparsec](https://github.com/mrkkrp/megaparsec) (Haskell)
- [FParsec](http://www.quanttec.com/fparsec/) (F#)
