# Module `Main` (TALM tool)

**Overview.** CLI that writes **TALM assembly** by running front-end and builder, then `Synthesis.Codegen.assemble`.

**Dependencies.** `Lexer`, `Parser`, `Syntax`, `Semantic`, `Synthesis.Builder`, `Synthesis.Codegen`, `Data.Text.Lazy.IO`.

**Function reference.**
- **`main :: IO ()`** — parse → check → build → `assemble` → stdout.
