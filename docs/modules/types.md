# Module `Types`

**Overview.** Core **graph** types and constructors for the dataflow IR.

**Exports.**
- `type NodeId = Int`
- `type PortId = String`
- `type Edge = (NodeId, PortId, NodeId, PortId)`
- `data DGraph n = DGraph { dgNodes :: Map NodeId n, dgEdges :: [Edge] }  deriving (Functor, Foldable, Traversable)`
- `emptyGraph :: DGraph n`
- `addNode :: NodeId -> n -> DGraph n -> DGraph n`
- `addEdge :: Edge -> DGraph n -> DGraph n`

**Dependencies.** `Data.Map`.

**Function reference.**
- **`emptyGraph`** — returns an empty `DGraph`.  
- **`addNode`** — insert a node.  
- **`addEdge`** — append an edge.
