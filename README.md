# HTC - Haskell TALM Compiler

Repository for the final undergraduate project presented at the Department of Computing, **UFES** (2nd semester 2025).  
**Author**: Ricardo Magalhães Santos Filho
**Advisor**: Prof. Dr. Alberto Ferreira de Souza  
**Co-advisor**: Prof. Dr. Tiago Assumpção de Oliveira Alves

Code documents: https://rickymagal.github.io/HTC/

## Introduction

This work proposes the translation of programs written in a Haskell subset into Dataflow graphs, bridging the functional paradigm—based on pure expressions—with a data-driven parallel execution model. The compiler automatically generates graphs compatible with distributed execution environments, with optional support for conversion to THLL (TALM High Level Language) used by TALM-based architectures.

## Repository Structure

| Directory         | Content                                                                                                                   |
|-------------------|---------------------------------------------------------------------------------------------------------------------------|
| `docs/`           | Project proposal (`anteprojeto.pdf`), monograph (`monografia.pdf`), and formal language definition (`haskell-subset.ebnf`). |
| `src/`            | Compiler source code.                                                                                                     |
| `test/`           | Unit and integration test cases.                                                                                          |
| `test/ast-images` | Generated AST images.                                                                                                     |
| `test/output`     | Generated `.dot` files.                                                                                                   |

## Execution

### Analysis step (generates AST images on test/ast-images and .dot files on test/output)


```bash
# From the root of the project, run:
make
```

## Dependencies

GHC (Glasgow Haskell Compiler)

Cabal (Haskell Package Manager)

Alex (Lexer generator)

Happy (Parser generator)

Haddock (Haskell documentation tool)

Graphviz (for rendering DOT graphs)

Make (build tool)
