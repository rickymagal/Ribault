# Module `Main` (Graph tool)

**Overview.** CLI that writes **Dataflow DOT**: runs front-end + `Synthesis.Builder.buildProgram` and prints `Synthesis.GraphViz.toDot`.

**Dependencies.** `Lexer`, `Parser`, `Syntax`, `Semantic`, `Synthesis.Builder`, `Synthesis.GraphViz`.

**Function reference.**
- **`main :: IO ()`** — parse → check → build → `toDot` → stdout.
