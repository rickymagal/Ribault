# Module `Synthesis.SuperExtract`

**Overview.** Traverses the AST (after `assignSuperNames`) to **collect all `super` blocks** and their metadata for FFI emission.

**Exports.**
- `data SuperSpec = SuperSpec { ssName :: Ident, ssKind :: SuperKind, ssInp :: Ident, ssOut :: Ident, ssBody :: String }`
- `collectSupers :: Program -> [SuperSpec]`

**Dependencies.** `Syntax`, `Data.List`.

**Function reference.**
- **`collectSupers :: Program -> [SuperSpec]`** — *Program → list of supers* with names, IO symbols and raw bodies.
