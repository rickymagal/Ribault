# TALM Language Guide

This guide explains how to write programs, what supers are, and how to
build/run a program with TALM. It reflects the current parser/lexer in
`src/Analysis/Parser.y` and `src/Analysis/Lexer.x`.

## 1) Program structure

A program is a list of function declarations separated by semicolons.
The last declaration can omit a trailing semicolon.

Example:
```
sum xs = case xs of
  []     -> 0;
  (h:ts) -> h + sum ts;
;

main = sum [1,2,3];
```

## 2) Expressions you can use

- Literals: `123`, `3.14`, `'a'`, `"str"`, `True`, `False`
- If: `if cond then e1 else e2`
- Let: `let x = ...; y = ... in expr`
- Case: `case expr of pat -> expr; ...`
- Lists: `[1,2,3]`, `x:xs`
- Tuples: `(a,b)`, `(a,b,c)`
- Function application: `f x y`
- Lambda: `\x -> expr` or `\(x,y) -> expr`

Operators and precedence are described in `docs/syntax_ebnf.md`.

## 3) Supers: what they are

**Supers** are Haskell blocks embedded in the language. They are executed
by the TALM interpreter as *super-instructions* and are compiled into a
shared library (`libsupers.so`).

Why use supers:
- to run a chunk of work in the host (Haskell) runtime
- to implement leaf kernels (e.g., merges, dot products)
- to bypass high overhead dataflow nodes when needed

Supers are *not* parsed by the TALM language parser. The lexer captures
the raw text between `#BEGINSUPER` and `#ENDSUPER` and stores it as the
super body.

Syntax:
```
name x =
  super single input (x) output (out)
#BEGINSUPER
    out = ...
#ENDSUPER;
```

Kinds:
- `super single`   (single-threaded intent)
- `super parallel` (parallel intent; scheduling handled by TALM)

**Input/output** are single identifiers; you can pass compound data by
packing it into lists or tuples.

### Helpers available inside supers

The generated `Supers.hs` provides:
- `toList` / `fromList` for list handles (Int64)
- `encPair`, `fstDec`, `sndDec` for pair-based list encoding

For floats inside lists, the generator also emits:
- `toFloat` / `fromFloat` (bitcast via low 32 bits)
- `toListF` / `fromListF`

Example (printing a list):
```
print_final xs =
  super single input (xs) output (out)
#BEGINSUPER
    out = unsafePerformIO
      (do
        print (toList xs)
        pure [0])
#ENDSUPER;
```

Example (float list):
```
print_f xs =
  super single input (xs) output (out)
#BEGINSUPER
    out = unsafePerformIO
      (do
        print (toListF xs)
        pure [0])
#ENDSUPER;
```

## 4) Building artifacts

There are four main artifacts:
- `.fl`   (flow assembly)
- `.flb`  (binary)
- `.pla`  (placement)
- `libsupers.so` (super-instructions library)

You can generate them manually (replace `/path/to/TALM` with your path):

```
./codegen program.hsk > program.fl
python3 /path/to/TALM/asm/assembler.py -n <P> -o program program.fl
./supersgen program.hsk > Supers.hs
CPPFLAGS=$(cat build/ghc-shim/.cppflags) C_INCLUDE_PATH=$(cat build/ghc-shim/.cpath) \
CPATH=$(cat build/ghc-shim/.cpath) GHC_LIBDIR=build/ghc-shim \
RTS_SO=$PWD/build/ghc-shim/rts/libHSrts-ghc$(ghc --numeric-version).so \
./tools/build_supers.sh program.hsk Supers.hs
```

This produces:
```
program.fl
program.flb
program.pla
libsupers.so
```

## 5) Running a program in TALM

```
/path/to/TALM/interp/interp <P> program.flb program.pla libsupers.so
```

Example:
```
TALM/interp/interp 1 program.flb program.pla libsupers.so
```

## 6) Generating from a new program (not in test/)

1) Write your `program.hsk`
2) Run the toolchain above (codegen + assembler + supersgen + build_supers)
3) Execute with `TALM/interp/interp`

You do not need to place the program under `test/` for this flow.

## 7) Notes and pitfalls

- The lexer treats everything between `#BEGINSUPER` and `#ENDSUPER` as raw
  Haskell code.
- If you want float correctness in lists, use `toFloat/fromFloat` or
  `toListF/fromListF`.
- If you do not call supers, you can omit `libsupers.so` in `interp`.

