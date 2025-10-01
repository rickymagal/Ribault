# Ribault - Haskell TALM Compiler

Repository for the final undergraduate project presented at the Department of Computing, **UFES** (2nd semester 2025).  
**Author**: Ricardo Magalhães Santos Filho
**Advisor**: Prof. Dr. Alberto Ferreira de Souza  
**Co-advisor**: Prof. Dr. Tiago Assumpção de Oliveira Alves

Code documents: https://rickymagal.github.io/Ribault

## Introduction

This work proposes the translation of programs written in a Haskell subset into Dataflow executable code, bridging the functional paradigm—based on pure expressions—with a data-driven parallel execution model. The compiler automatically generates graphs compatible with distributed execution environments, with additional support for generating TALM assembly code.

## Repository Structure

| Directory         | Content                                                                                                                   |
|-------------------|---------------------------------------------------------------------------------------------------------------------------|
| `src/Analysis`    | Compiler's analysis step source code.                                                                                       |
| `src/Synthesis`   | Compiler's synthesis step source code.                                                                                                     |
| `test/`           | Unit and integration test cases.                                                                                          |
| `test/ast-images` | Generated AST images.                                                                                                     |
| `test/df-images`  | Generated Dataflow graph images.                                                                                          |
| `test/talm`       | Generated TALM code.                                                                                                     |
| `test/supers/`    | Generated shared library binaries for user-defined super instructions                                                                                                  |
| `test/golden-df`  | Standard for Dataflow graphs .dot.                                                                                          |
| `test/golden-ast` | Standard for AST .dot.                                                                                          |
| `test/golden-talm` | Standard for TALM code.                                                                                          |
| `scripts/`          | Performance and correctness scripts                                                                                  |
| `tools/`            | Auxiliary scripts for compiling supers defined by users as a shared library                                                                                |

## Execution

```bash
# From the root of the project, run:
make
