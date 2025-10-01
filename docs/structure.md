# Repository Structure

| Path                 | What it contains                                                                 |
|----------------------|-----------------------------------------------------------------------------------|
| `src/Analysis/`      | Front-end: AST types (`Syntax`), semantic/type checker (`Semantic`), AST DOT (`ASTGen`), Alex/Happy sources. |
| `src/Synthesis/`     | Back-end: dataflow types/utilities (`Types`, `Port`, `Node`), graph builder (`Synthesis.Builder`), DOT renderer (`Synthesis.GraphViz`), TALM emitter (`Synthesis.Codegen`), Super extraction/emission. |
| `scripts/`           | Python helpers for generating inputs, running variants, plotting results.        |
| `tools/`             | Utilities for the Super library build (e.g., RTS init shim, alias script).       |
| `test/`              | Input `.hsk` programs, golden baselines, and generated outputs.                  |
| `build/ghc-shim/`    | Local RTS shim created by the Makefile for Super `.so` linking.                  |
