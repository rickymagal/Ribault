# Module `Synthesis.Codegen`

**Overview.** **Dataflow → TALM assembly** emission.

**Exports.**
- `assemble :: DGraph DNode -> T.Text`

**Dependencies.** `Types`, `Node`, `Port`, `Data.Text`, `Data.Map`.

**Function reference.**
- **`assemble :: DGraph DNode -> T.Text`** — walks the graph, assigns tags to call groups, prints instruction mnemonics per `DNode` and formats multi-input operands.
