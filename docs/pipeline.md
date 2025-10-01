# Compiler Pipeline

**Front-end**  
1) **Lexing** → `Lexer` (Alex) turns source text into tokens.  
2) **Parsing** → `Parser` (Happy) builds an **AST** (`Syntax.Program`).  
3) **Semantic** → `Semantic.checkAll` runs semantic checks and HM-style type inference; also **assigns names** to `super` blocks (`assignSuperNames`).

**IR & Back-end**  
4) **AST → DOT** → `ASTGen.programToDot` emits Graphviz DOT for AST (inspection).  
5) **AST → Dataflow** → `Synthesis.Builder.buildProgram :: Program -> DFG` constructs a typed, explicit **dataflow graph** (`Types.DGraph DNode`).  
6) **Dataflow → DOT** → `Synthesis.GraphViz.toDot` renders the dataflow graph to DOT.  
7) **Dataflow → TALM** → `Synthesis.Codegen.assemble` lowers the graph to **TALM assembly** (text).  
8) **Supers** → `Synthesis.SuperExtract.collectSupers` + `Synthesis.SupersEmit.emitSupersModule` generate a Haskell FFI module exporting `sN` entry points, which the Makefile turns into a shared library under `test/supers/`.
