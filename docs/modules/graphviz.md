# Module `Synthesis.GraphViz`

**Overview.** Pretty-printer: **dataflow graph → DOT** (Graphviz).

**Exports.**
- `toDot :: DGraph DNode -> TL.Text`

**Dependencies.** `Types`, `Node`, `Data.Text`/`Lazy`, `Data.Map`.

**Function reference.**
- **`toDot :: DGraph DNode -> TL.Text`** — prints nodes/edges deterministically and formats operator labels.
