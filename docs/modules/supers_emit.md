# Module `Synthesis.SupersEmit`

**Overview.** Generates the Haskell module (`Supers.hs`) exporting C-callable `sN` functions (FFI) for each collected `super` (ABI profile B).

**Exports.**
- `emitSupersModule :: String -> [SuperSpec] -> String`

**Dependencies.** `Synthesis.SuperExtract`, `Data.Char`, `Data.List`.

**Function reference.**
- **`emitSupersModule base specs`** — builds header/imports, encoder/decoder helpers and a `foreign export ccall "sN"` for each spec. Emits empty string if no supers to skip `.so` build.
