# Module `Synthesis.Builder`

**Overview.** The **IR builder**: lowers a checked `Syntax.Program` into a **dataflow graph**.

**Exports.**
- `type DFG = DGraph DNode`
- `buildProgram :: Program -> DFG`

**Dependencies.** `Syntax`, `Types`, `Node`, `Port`, `Unique`, `Data.Map`, `Data.Set`, `Control.Monad.State`.

**Function reference.**
- **`buildProgram :: Program -> DFG`** — traverses declarations, allocates fresh `NodeId`s, emits nodes for expressions and wires edges; creates per-function input/output nodes and enforces arity.
