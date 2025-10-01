# Requirements & Build

## Exact Requirements

To build and run everything (AST/DF graphs, TALM, and Super library):

- **GHC 9.6.x** (tested with 9.6.6 on Fedora).
- **make**
- **Graphviz** (`dot`) — to render `.dot` into `.png`.
- **Python 3** — for helper scripts under `scripts/`.

For *developing* the scanner/parser (only if you will regenerate them):

- **Alex** (for `src/Analysis/Lexer.x`)
- **Happy** (for `src/Analysis/Parser.y`)

> The Makefile also builds a **local GHC RTS shim** (`build/ghc-shim`) so the Super-library (`.so`) links against the correct RTS/base even on distros with non-standard layouts.

## One-liner Build

```bash
make          # builds: dataflow, AST, TALM, and supers (shared lib)
```

Targets:

- `make df` — dataflow DOT + PNG for all `test/*.hsk`
- `make ast` — AST DOT + PNG
- `make code` — TALM assembly
- `make supers` — generates `Supers.hs` (from program `super` blocks) and builds a `.so` under `test/supers/`
- `make clean` — remove build artifacts and outputs

**Executables** produced in the repo root: `synthesis` (DF), `analysis` (AST), `codegen` (TALM), `supersgen` (Super module emitter).
