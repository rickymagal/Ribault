# Ribault — Haskell to Dataflow (TALM) Compiler

**Ribault** compiles a subset of Haskell into a **coarse-grained dataflow** representation and **TALM** (Trebuchet) assembly. It is the codebase for the author’s CS capstone (UFES).

Goals:

- Keep the **front-end** close to Haskell’s functional style (expressions, lists, pattern-matching),
- Lower to an explicit **dataflow graph** suitable for Trebuchet/TALM,
- Provide **inspection artifacts** (Graphviz DOT for AST and Dataflow),
- Emit **TALM assembly** and a **Super-instruction library**.
