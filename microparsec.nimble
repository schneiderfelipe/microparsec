# Package

version       = "0.1.0"
author        = "Felipe S. S. Schneider"
description   = "A performant Nim parsing library built for humans."
license       = "MIT"
srcDir        = "src"

# Dependencies

requires "nim >= 1.2.6"
requires "result >= 0.2.0"

# Tasks

task docs, "Generate documentation":
  exec "nim doc --project --index:on --git.url:https://github.com/schneiderfelipe/microparsec --git.commit:master --outdir:docs src/microparsec.nim"
  exec "ln -s microparsec.html docs/index.html || true"
