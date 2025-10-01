# Module `Main` (AST tool)

**Overview.** CLI that reads a Haskell-subset program and writes **AST DOT** to stdout when `Semantic.checkAll` passes.

**Dependencies.** `Lexer`, `Parser`, `Syntax`, `Semantic`, `ASTGen`, `Data.Text.Lazy.IO`.

**Function reference.**
- **`main :: IO ()`** — parse → check → `ASTGen.programToDot` → stdout; on errors, prints to `stderr` and exits non-zero.
