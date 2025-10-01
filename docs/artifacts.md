# Executables & Outputs

## Executables (built in repo root)

- **`analysis`** — reads a `.hsk`, runs front-end and semantic checks, writes **AST DOT** to `test/ast-output/*.dot`. The Makefile turns DOT → PNG under `test/ast-images/`.
- **`synthesis`** — same front-end; builds **dataflow graph** and writes **DF DOT** to `test/df-output/*.dot`. Makefile renders PNGs to `test/df-images/`.
- **`codegen`** — same front-end; builds DF and emits **TALM** (`.fl`) to `test/code-output/*.fl`.
- **`supersgen`** — reads a `.hsk`, collects named `super` blocks and emits a `Supers.hs` module to stdout; the Makefile captures it and builds a **shared library** under `test/supers/` (with the RTS shim), exporting `sN` entry points via FFI.

## Output directories

- `test/ast-output/*.dot` and `test/ast-images/*.png` — AST graphs.
- `test/df-output/*.dot` and `test/df-images/*.png` — Dataflow graphs.
- `test/code-output/*.fl` — TALM assembly.
- `test/supers/` — the Super library `.so` plus intermediates.
