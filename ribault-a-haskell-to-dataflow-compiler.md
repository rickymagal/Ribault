# Ribault: a Haskell-to-Dataflow Compiler for High-Performance Parallelism

> *Intuitively explained to those who wish to abuse it.*

**Ricardo Magal** — March 26, 2026

---

[Ribault](https://github.com/tiagoaoa/ribault-release) is a Haskell-to-Dataflow compiler: it takes Haskell code and compiles it into TALM (TALM is an Architecture and Language for Multi-Threading), a dataflow architecture developed by Prof. Dr. Tiago Alves, achieving up to **7.01x faster execution** than GHC's best parallel variant at the same thread count. The dataflow model itself, developed by Dennis and Arvind at MIT, is what we dubbed the truly parallel model: one that imposes no restrictions on a problem beyond those that exist in the very nature of the problem itself; one that makes the maximum degree of concurrency of an algorithm evident and intuitive. This text will introduce the dynamic dataflow model (which is the variation we use here), make very clear the reason for choosing Haskell as the source language, cover an example in the defined language subset, address granularity and super-instructions (which are what guarantee Ribault's performance on conventional hardware), close with some results obtained, and extend an open invitation to collaborate on the improvements already on the horizon.

![Ribault](https://substack-post-media.s3.amazonaws.com/public/images/b6e8e4a6-8957-4c82-aa64-846a83369e01_800x421.jpeg)

---

## Dynamic Dataflow

In dataflow, program execution is driven by data availability. An operation fires as soon as every input operand has arrived; no further synchronisation is required. Beautiful, elegant and simple.

A dataflow program can be represented as a directed graph: each node stands for an operation, and each edge represents a data dependency between two of those operations. The structure of the graph itself dictates what can happen and when.

The foundations of the model were laid by Karp and Miller, who formalised what they called computation graphs, establishing results about the determinacy and termination of such systems. Building on that work, Dennis later developed the notion of a firing rule and a token-passing vocabulary that remains in use to this day.

Determinacy emerges naturally: although the data dependencies constrain which nodes may fire at any given moment, any execution order that respects those dependencies will produce the same final result. But perhaps the most important consequence is implicit parallelism: any two nodes between which there is no directed path are, by definition, free to execute at the same time. The programmer does not need to declare what can be parallelised, since that information is already in the graph itself.

Classical dataflow graphs are acyclic: they flow in one direction and never loop back on themselves. This makes scheduling and analysis easier: one can determine the critical path, bound execution time, and reason about parallelism without concern for how repeated traversals might interact. But this comes at a price: loops and recursion cannot be represented directly.

Dynamic dataflow graphs address this limitation by introducing a distinction between two kinds of dependency. Some edges carry data within a single execution of a loop body — one instruction feeding another in the same iteration, exactly as in the classical model. Others carry data across iterations, where the output of one pass becomes the input of the next. It is this second kind that creates cycles in the graph, and with them the ability to model repetition natively.

By unrolling the graph, or replicating the loop body a fixed number of times and making the cross-iteration dependencies explicit as ordinary edges, one recovers an acyclic graph that is fully amenable to classical analysis. The cycle is just a compact notation for a long chain of ordinary dependencies.

---

## Why Haskell?

The dataflow model and the functional programming paradigm based on the lambda calculus share a common intuition. McCarthy, building Lisp, and Dennis, formalising dataflow, were both searching for what is *essential* to a program, stripped of the accidents of how computers happen to be built. McCarthy's answer was the function — a mapping from a domain set to its corresponding image. Dennis's answer was the node — an operation that fires when its operands arrive and produces an answer. A computation starts to be viewed as a relationship between inputs and outputs, and nothing else.

That shared intuition turns out to be more than an epistemological. A purely functional program *is*, in a precise structural sense, a dataflow graph. Every expression takes some inputs and produces an output that depends on nothing else — which is exactly what a dataflow node does. Two expressions with no shared variables have no dependency between them, which means they correspond to two nodes with no edge between them, and are therefore unconditionally eligible for parallel execution. But this graph does not need to be constructed by hand or inferred by a clever analysis pass. It is already there, implicit in the syntax of the program, waiting to be read off.

This is what referential transparency actually buys you in practice. It is not just a theoretical nicety or a discipline that makes code easier to reason about. It is the guarantee that the compiler can look at any expression and know, with certainty, exactly what it depends on and exactly what it affects — simply because the answer to the second question is always "nothing beyond its return value." That guarantee is what makes the translation from source program to dataflow graph natural and complete.

But Haskell takes this further than most. Its type system enforces purity. Side effects are tracked by the type system and must be declared explicitly. This means that when the Ribault compiler looks at a block of Haskell code, the absence of hidden dependencies is not an assumption that could be violated somewhere deep in a library call. It is a fact, certified before compilation even begins. The graph the compiler builds is the graph the program actually describes.

---

## Hsub

Hsub is a strict, purely functional language; a Turing-complete subset of Haskell. A program is a collection of mutually recursive function declarations. Expressions are what one would expect from a functional language: lambda abstractions, conditionals, pattern matching via `case`, local recursive bindings via `let`, binary and unary operators, lists, pairs, and function application.

An example of a Merge Sort algorithm implemented in Hsub is as follows:

```haskell
-- Merge Sort in the compiled functional subset

len xs = case xs of
  []     -> 0
  (_:ys) -> 1 + len ys

p = 4

merge xs ys = case xs of
  [] -> ys
  (x:xt) -> case ys of
    [] -> xs
    (y:yt) -> if x <= y
              then x : merge xt ys
              else y : merge xs yt

splitCount lst = case lst of
  [] -> ([], ([], (0, 0)))
  (x:[]) -> ([x], ([], (1, 0)))
  x:y:zs ->
    case splitCount zs of
      (xs, (ys, (nl, nr))) -> (x:xs, (y:ys, (nl + 1, nr + 1)))

mergeSort0 lst =
  let n0 = len lst
  in mergeSortT n0 n0 lst

mergeSortT n0 n lst = case lst of
  []     -> []
  (x:[]) -> [x]
  _      ->
    if n <= (n0 / p)
    then super seqSort lst (
           seqSort xs = fromList (Data.List.sort (toList xs))
         )
    else
      case splitCount lst of
        (left, (right, (nl, nr))) ->
          merge (mergeSortT n0 nl left) (mergeSortT n0 nr right)

main = mergeSort0 [34,7,23,32,5,62,1,9,8,4,3,11]
```

The divide-and-conquer recursion splits the list in parallel until the sublist size `n` reaches `n0 / p`, at which point it stops forking and hands the remaining work to an inline super-instruction that simply calls `Data.List.sort` on the sublist. The reason this cutoff must exist at all is the coordination cost of the dataflow model itself: every super-instruction invocation carries a fixed token-passing overhead, and if the recursion were allowed to fork all the way down to single elements, that overhead would be paid at every leaf of the tree for a trivial amount of sequential work, drowning the computation in coordination noise and collapsing performance below the sequential baseline. The cutoff is therefore the mechanism that ensures each super invocation receives a sublist large enough that the sequential work it performs comfortably exceeds the cost of firing it — and `n0 / p`, the total input size divided by the parallelism factor `p = 4`, is what controls exactly where that transition happens.

![Call graph for merge sort with p = 4](https://substack-post-media.s3.amazonaws.com/public/images/79981b0a-b05a-45f2-adf5-15c53b34fd8a_1840x1561.png)

**Fig.** Call graph for merge sort with `p = 4`. The recursion forks in parallel down the tree until `n ≤ n0 / p`, at which point each branch stops splitting and emits a leaf super-instruction (`S1`–`S4`) that sorts its sublist entirely in sequential native code. With `p = 4`, exactly four such leaves are produced, all of which are data-independent and therefore fire concurrently. Once all four complete, the merge nodes reduce the results back up the tree sequentially by dependency.

---

## The Ribault Compiler

Ribault is a compiler from Hsub to dataflow graphs targeting the TALM runtime. It takes annotated Haskell code and produces two things: a TALM assembly file describing the dataflow graph, and a native shared library containing the sequential Haskell computations that live inside that graph. The pipeline has four phases.

The front-end parses the source into an abstract syntax tree. Semantic analysis then type-checks it, resolves free variables, and performs lambda-lifting, making all free variables in lambda abstractions explicit so that every function can be treated as a closed term. Graph construction traverses the typed tree and produces the dataflow graph: literals and variables become constant or argument nodes, conditionals become steering subgraphs, recursive calls create back-edges tagged with the call/return protocol, and super-instruction blocks become opaque nodes. Code generation then serialises the graph to TALM assembly and extracts the super-instruction bodies into a separate Haskell module, compiled by GHC with full optimisations into the shared library that the runtime loads at execution time.

![The Ribault compiler pipeline](https://substack-post-media.s3.amazonaws.com/public/images/2706d1b5-680a-4fa5-9d54-97e942be5410_2536x637.png)

**Fig.** *The Ribault compiler pipeline. Source code enters on the left and passes through four phases — parsing, semantic analysis, graph construction, and code generation — exiting as a TALM assembly file and a GHC-compiled shared library of super-instructions.*

The Fibonacci example makes the whole pipeline concrete. The source has no parallelism annotations whatsoever; just a straightforward recursive definition. The compiler reads the dependency structure: `fib (n-1)` and `fib (n-2)` share no directed path in the graph, so they are unconditionally parallel. The emitted assembly reflects this directly, with `callsnd`/`retsnd` pairs dispatching both recursive calls simultaneously and a single `add` node waiting for both results before firing. The programmer stated the recurrence; the parallelism was already there.

![End-to-end compilation of recursive Fibonacci](https://substack-post-media.s3.amazonaws.com/public/images/f472a808-d54f-4738-8f90-3bb94678db4c_626x881.png)

**Fig.** *End-to-end compilation of recursive Fibonacci. (A) Hsub source with no parallelism annotations. (B) The compiled dataflow graph: solid edges carry data tokens; dashed edges carry recursive call/return tokens. The two recursive subgraphs share no directed path and are unconditionally parallel. (C) TALM assembly: one instruction per node; `callsnd`/`retsnd` pairs implement the call/return protocol; `steer` routes tokens rather than jumping to an address.*

The central design decision the programmer must make is granularity. Every super-instruction invocation carries a fixed coordination cost. When a super encapsulates enough sequential work, this cost disappears into the noise and parallelism dominates. When super bodies are too fine-grained, coordination overhead exceeds the computation and performance collapses below the sequential baseline. The practical rule of thumb is that each super should encapsulate at least milliseconds of sequential work. Divide-and-conquer algorithms satisfy this naturally as input size grows; the merge sort and self-attention examples make this explicit.

This TALM assembly is simulated on your computer by the Trebuchet runtime. Why does simulating dataflow on conventional hardware beat a native parallel runtime at its own game? There is no global scheduler negotiating thread priorities, no shared heap requiring coordinated garbage collection pauses, no spark pool whose work-stealing heuristics may or may not expose the parallelism the programmer intended. Each super is a self-contained sequential process that communicates exclusively through token channels; when it finishes, it deposits tokens and becomes irrelevant to every other super that does not directly consume those tokens. This is the deeper meaning of the truly parallel model: the runtime does exactly as much coordination as the problem requires and structurally cannot do more. GHC's parallel runtime, by contrast, must conservatively synchronize around a shared heap, express pipeline stages as speculative sparks whose dependency on one another is invisible to the scheduler, and pay for stop-the-world collection across all threads simultaneously. Ribault does not win by being clever about scheduling; it wins because the execution model it targets has fewer inherent synchronization obligations than the one GHC targets, and fewer obligations mean less waiting.

On parallel text search, Ribault reaches 8.13× speedup at 16 threads while GHC's best parallel variant plateaus below 1.21× — collapsing under stop-the-world garbage collection pressure that Ribault avoids by construction, since each super runs in its own thread with its own heap. On LCS wavefront, the barrier-free firing rule outpaces GHC Strategies at every configuration and leaves GHC par/pseq's explicit diagonal barriers entirely behind. On self-attention with a two-phase I/O–compute pipeline, Ribault reaches 8.16× against 2.94× for the best GHC variant, exploiting pipeline overlap across blocks that is structurally inexpressible in GHC's spark model, because in Ribault the dependency between loading and computing is simply an edge in the graph, and the runtime does the rest.

---

## An Open Invitation

Ribault is working, benchmarked, and open. But it is also, frankly, a first step. A complete first step, but a first step nonetheless. The directions forward are clear enough that listing them feels less like admitting limitations and more like describing a research agenda that is genuinely worth pursuing.

The first direction is extending Hsub itself. Type classes, higher-order functions as first-class dataflow values, richer pattern matching — all of these would broaden the class of programs that Ribault can compile without forcing the programmer into a restricted subset.

The second is distribution. Trebuchet today runs on a single multicore machine. The Sucuri project, which shares much of Ribault's intellectual lineage, already demonstrated dataflow execution across a cluster. Mapping Ribault's graphs onto distributed Trebuchet instances is a natural next step: the firing rule is local by construction, and locality is exactly what makes distribution tractable.

Finally, the proofs. Type preservation and semantic preservation are established on paper. Mechanising them in Rocq or Agda would shrink the trusted base and make Ribault something stronger than a fast compiler: it would make it an automatically verified one.

If any of this is in your wheelhouse, the [code](https://github.com/tiagoaoa/ribault-release) is available. We would be glad to hear from you.
