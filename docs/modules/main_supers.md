# Module `MainSupers`

**Overview.** CLI that emits a Haskell FFI module (`Supers.hs`) from named `super` blocks, or nothing if there are none.

**Dependencies.** `Lexer`, `Parser`, `Semantic` (`checkAll`, `assignSuperNames`), `Syntax`, `Synthesis.SuperExtract`, `Synthesis.SupersEmit`.

**Function reference.**
- **`main :: IO ()`** — parse → check → `assignSuperNames` → `collectSupers` → `emitSupersModule` → stdout.
