# Ribault - Haskell TALM Compiler

Repository for the final undergraduate project presented at the Department of Computing, **UFES** (2nd semester 2025).
**Author**: Ricardo Magalhaes Santos Filho
**Advisor**: Prof. Dr. Alberto Ferreira de Souza
**Co-advisor**: Prof. Dr. Tiago Assumpcao de Oliveira Alves

Code documents: https://rickymagal.github.io/Ribault

## Introduction

This work proposes the translation of programs written in a Haskell subset into Dataflow executable code, bridging the functional paradigm—based on pure expressions—with a data-driven parallel execution model. The compiler automatically generates graphs compatible with distributed execution environments, with additional support for generating TALM assembly code.

## Repository Structure

| Directory | Content |
|---|---|
| `src/Analysis` | Parsing + analysis stages (Lexer/Parser/AST). |
| `src/Synthesis` | Dataflow + TALM code generation. |
| `test/` | Unit/integration test programs (`*.hsk`). |
| `test/ast-output` | AST `.dot` generated from `analysis`. |
| `test/ast-images` | AST `.png` rendered from `.dot`. |
| `test/df-output` | Dataflow `.dot` generated from `synthesis`. |
| `test/df-images` | Dataflow `.png` rendered from `.dot`. |
| `test/talm` | TALM `.fl` generated from `codegen`. |
| `test/supers/` | Per-program `Supers.hs` + `libsupers.so`. |
| `scripts/` | Performance/correctness scripts. |
| `tools/` | Supers build helpers, alias/fix scripts. |
| `TALM/` | Trebuchet interpreter + assembler (vendored). |
| `build/` | GHC shim for RTS discovery. |

## Build / Targets

Prerequisites: `ghc`, `alex`, `happy`, `dot` (Graphviz), `python3`, `gcc`.
The TALM interpreter/assembler is under `TALM/`.

```bash
# Build everything for test programs
make

# Generate specific artifacts for tests
make ast      # AST .dot + .png
make df       # Dataflow .dot + .png
make code     # TALM .fl
make supers   # Supers.hs + libsupers.so
```

Executables produced:
- `analysis`  -> AST `.dot`
- `synthesis` -> Dataflow `.dot`
- `codegen`   -> TALM `.fl`
- `supersgen` -> Supers Haskell for super-instructions

## Generating Artifacts for a New Program (outside `test/`)

Assuming a new file `myprog.hsk`:

```bash
./analysis  /path/to/myprog.hsk > /tmp/myprog.ast.dot
./synthesis /path/to/myprog.hsk > /tmp/myprog.df.dot
./codegen   /path/to/myprog.hsk > /tmp/myprog.fl

# Render images (optional)
dot -Tpng /tmp/myprog.ast.dot -o /tmp/myprog.ast.png
dot -Tpng /tmp/myprog.df.dot  -o /tmp/myprog.df.png
```

Generate supers + shared lib for that program:

```bash
mkdir -p /tmp/myprog_supers
tools/build_supers.sh /path/to/myprog.hsk /tmp/myprog_supers/Supers.hs
# Output: /tmp/myprog_supers/libsupers.so (+ ghc-deps/)
```

## Running on TALM (Trebuchet)

The TALM path can vary per user. Use `TALM_DIR`:

```bash
TALM_DIR=/path/to/TALM
P=8

# Assemble
python3 "$TALM_DIR/asm/assembler.py" -a -n "$P" -o /tmp/myprog /tmp/myprog.fl

# Run
"$TALM_DIR/interp/interp" "$P" \
  /tmp/myprog.flb \
  /tmp/myprog_auto.pla \
  /tmp/myprog_supers/libsupers.so
```

Notes:
- `-a` enables autoplacement (generates `*_auto.pla`).
- `-n P` defines the number of processing elements (PEs).

## Build Flags / Options

Makefile variables (override on the command line):

```bash
GHC=ghc-9.6.4 ALEX=alex HAPPY=happy DOT=dot make
```

Supers-specific flags:

```bash
SUPERS_THREADED=1        # default: 1 (link against threaded RTS)
SUPERS_WRAPPERS_MAX=256  # number of superN wrappers exported
make supers
```

Other useful overrides (advanced):
- `SUPERS_LINK_FLAGS` (custom link flags for `libsupers.so`)
- `TESTS` (limit which `test/*.hsk` are built)

## Design Decisions (catalog)

- **Dataflow graph is the source of truth**: `.dot` (Dataflow) and `.fl` (TALM) represent the same graph; `.fl` is the executable form for Trebuchet.
- **Function calls are explicit in TALM**: calls use `callgroup` + `callsnd`/`retsnd` + `ret` to carry tags and execution IDs correctly across call instances.
- **Supers are compiled as shared libraries**: `libsupers.so` is produced with `-no-hs-main` and an RTS shim. Dependencies are bundled under `ghc-deps/` and loaded via rpath.
- **Threaded RTS by default**: `SUPERS_THREADED=1` to allow parallel execution in Trebuchet.
