# Module `Port`

**Overview.** Port abstraction and safe connectors for building edges.

**Exports.**
- `data Port = ...`
- `portNode :: Port -> NodeId`
- `portId :: Port -> PortId`
- `(--> ) :: Port -> Port -> Edge`
- `edge :: NodeId -> PortId -> NodeId -> PortId -> Edge`

**Dependencies.** `Types`.

**Function reference.**
- **`portNode`** — extract origin node id.  
- **`portId`** — extract port label.  
- **`(--> )`** — sugar for edge construction.  
- **`edge`** — explicit edge constructor.
