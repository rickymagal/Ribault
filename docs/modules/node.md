# Module `Node`

**Overview.** The **dataflow node** payload used in `DGraph`: constants, arithmetic ops, comparisons/steer, control, and TALM call/return nodes.

**Exports.**
- `data DNode = ...` — e.g., `NConstI`, `NAdd`, `NSteer`, `NCallGroup`, `NCallSnd`, `NRetSnd`, `NCommit`, `NStopSpec`, …
- `nodeName :: DNode -> String`
- `nOutputs :: DNode -> Int`
- `outPort :: NodeId -> Port`, `out1Port :: NodeId -> Port`
- `truePort :: NodeId -> Port`, `falsePort :: NodeId -> Port`

**Dependencies.** `Types`, `Port`.

**Function reference.**
- **`nodeName`** — printed mnemonic.  
- **`nOutputs`** — number of output ports.  
- **`outPort/out1Port/truePort/falsePort`** — helpers to wire edges.
