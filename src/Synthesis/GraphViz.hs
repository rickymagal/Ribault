{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Synthesis.GraphViz
-- Description : Render a 'DGraph' of 'DNode's to GraphViz DOT.
-- Maintainer  : ricardofilhoschool@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Pretty-prints a dataflow graph to DOT (GraphViz). Two modes:
--
-- * 'toDot' — full graph with all nodes (infrastructure included).
-- * 'toCleanDot' — semantic-only graph, omitting tag infrastructure
--   and reconnecting edges through removed nodes.

module Synthesis.GraphViz (toDot, toCleanDot, toFormalDot) where

import qualified Data.Map       as M
import qualified Data.Set       as S
import           Data.Char      (isDigit)
import           Data.List      (sortOn, nub, isPrefixOf)
import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL
import           Data.Text      (Text)

import           Types          (DGraph(..), NodeId)
import           Node           (DNode(..))

--------------------------------------------------------------------------------
-- Full graph (unchanged)
--------------------------------------------------------------------------------

-- | Convert a graph of 'DNode's to a DOT 'TL.Text' (full, all nodes).
toDot :: DGraph DNode -> TL.Text
toDot g =
  TL.fromStrict . T.unlines $
    [ "digraph G {"
    , "  rankdir=LR;"
    , "  node [shape=box, style=rounded, fontsize=12];"
    ]
    ++ map (ppNode g) (sortedNodes g)
    ++ map ppEdge        (sortedEdges g)
    ++ [ "}" ]

--------------------------------------------------------------------------------
-- Clean graph
--------------------------------------------------------------------------------

-- | Convert a graph to a clean DOT, omitting infrastructure nodes and
-- reconnecting edges through them.
toCleanDot :: DGraph DNode -> TL.Text
toCleanDot g =
  let nodes    = M.toList (dgNodes g)
      edges    = [ (s, T.pack sp, d, T.pack dp) | (s,sp,d,dp) <- dgEdges g ]
      -- Detect zeroing subs: NSub where both inputs come from same source node
      inEdges  = M.fromListWith (++)
                   [ (d, [s]) | (s, _sp, d, _dp) <- edges ]
      zeroSubs = S.fromList
                   [ nid | (nid, NSub{}) <- nodes
                         , let srcs = M.findWithDefault [] nid inEdges
                         , not (null srcs)
                         , S.size (S.fromList srcs) == 1 ]
      -- Detect constFrom pattern: NAddI fed by a zeroing sub = program constant
      progConsts = S.fromList
                     [ nid | (nid, NAddI{}) <- nodes
                           , let srcs = M.findWithDefault [] nid inEdges
                           , any (`S.member` zeroSubs) srcs ]
      -- Forward adjacency (node IDs only) for checking constFrom targets
      fwdAll   = M.fromListWith (++) [ (s, [d]) | (s,_,d,_) <- dgEdges g ]
      nodeMap  = dgNodes g
      -- Detect infra-adds: NAdd where ALL inputs come from infra,
      -- conditional, or zeroing-sub nodes.  Covers MUX joins
      -- (add steered_true + steered_false) and constFrom variants
      -- (zeroing-sub + valtag).  Remove so they don't show as unary ops.
      infraOrCond = S.fromList [ nid | (nid, dn) <- nodes
                                     , isInfra dn || isConditional dn ]
      allInfra    = S.union infraOrCond zeroSubs
      joinAdds = S.fromList
                   [ nid | (nid, NAdd{}) <- nodes
                         , let srcs = M.findWithDefault [] nid inEdges
                         , not (null srcs)
                         , all (`S.member` allInfra) srcs ]
      -- Base kept set (without constFrom constants or join-adds)
      keptBase = S.fromList [ nid | (nid, dn) <- nodes
                                  , not (isInfra dn)
                                  , not (S.member nid zeroSubs)
                                  , not (S.member nid joinAdds) ]
      -- Only keep constFrom constants that directly feed a keptBase node
      -- (e.g., const 1 → NSub, const 2 → NLThan). Exclude guardToken constants.
      semanticConsts = S.fromList
                         [ nid | nid <- S.toList progConsts
                               , let succs = M.findWithDefault [] nid fwdAll
                               , any (`S.member` keptBase) succs ]
      kept     = S.union keptBase semanticConsts
      -- Build final bypass edges
      fwdAdj   = M.fromListWith (++)
                   [ (s, [(sp, d, dp)]) | (s, sp, d, dp) <- edges
                                        , not (S.member s kept) ]
      cleanEdges = nub $ concatMap (resolveEdge kept fwdAdj) edges
      -- Remove self-loops, edges INTO progConsts (constFrom artifact); dedup
      deduped    = M.toList $ M.fromListWith pickLabel
                     [ ((s, d), sp) | (s, sp, d, _dp) <- cleanEdges, s /= d
                                    , not (S.member d semanticConsts) ]
      finalEdges0 = [ (s, sp, d) | ((s, d), sp) <- deduped ]
      -- Label edges from comparison nodes: T → NRet (return), F → everything else
      labeled = map labelCond finalEdges0
      labelCond (s, sp, d)
        | isConditional (lkNode s) , isRetNode (lkNode d) = (s, "T", d)
        | isConditional (lkNode s)                        = (s, "F", d)
        | otherwise                                       = (s, sp, d)
      lkNode nid = M.findWithDefault (NConstI "" 0) nid nodeMap
      isRetNode NRet{} = True
      isRetNode _      = False
      -- Transitive reduction: remove a→c if ∃ b with a→b and b→c.
      -- Preserves T/F labeled edges unconditionally.
      reduced = transitiveReduce S.empty labeled
      -- Chain return nodes: if non-ret X feeds both ret_inner and ret_outer,
      -- remove X→ret_outer and add ret_inner→ret_outer.
      chained    = chainReturns nodeMap reduced
      finalEdges = transitiveReduce S.empty chained
      -- Only keep nodes that participate in at least one edge
      edgeNodes  = S.fromList $ concatMap (\(s,_,d) -> [s,d]) finalEdges
      cleanNodes = sortOn fst [ (nid, dn) | (nid, dn) <- nodes
                                           , S.member nid kept
                                           , S.member nid edgeNodes ]
  in TL.fromStrict . T.unlines $
       [ "digraph G {"
       , "  rankdir=TB;"
       , "  node [fontsize=12];"
       ]
       ++ map (ppCleanNode progConsts) cleanNodes
       ++ map ppCleanEdge (sortOn (\(s,_,d) -> (d,s)) finalEdges)
       ++ [ "}" ]

-- | Infrastructure nodes to filter out.
isInfra :: DNode -> Bool
isInfra = \case
  NValTag{}       -> True
  NTagVal{}       -> True
  NBand{}         -> True
  NCommit{}       -> True
  NStopSpec{}     -> True
  NSteer{}        -> True          -- steers removed; T/F goes on comparison node
  NIncTag{}       -> True
  NIncTagI{}      -> True
  -- Tag manipulation immediates (small positives are tag offsets)
  NAddI{iImm=k}  -> k >= 0 && k <= 7
  NSubI{iImm=k}  -> k >= 0 && k <= 7
  NMulI{iImm=k}  -> k == 8       -- tag radix
  NDivI{iImm=k}  -> k == 8       -- tag radix
  NEqual{}         -> True         -- case/list dispatch
  NGThan{}         -> True         -- list length guard
  NGThanI{}        -> True
  NConstI{cInt=k}   -> k == 0 || k == 1  -- comparison constants
  -- List builtin supers (s0=cons, s1=head, s2=tail, s3=isNil)
  NSuper{superNum=sn} -> sn <= 3
  _               -> False

-- | Is this a user-level conditional (comparison) node?
isConditional :: DNode -> Bool
isConditional NLThan{}  = True
isConditional NLThanI{} = True
isConditional _         = False



-- | Resolve a single edge: if both endpoints are kept, pass through.
-- If source is kept but dest is removed, BFS through removed nodes.
-- If source is removed, skip (handled by predecessor's BFS).
resolveEdge :: S.Set NodeId
            -> M.Map NodeId [(Text, NodeId, Text)]
            -> (NodeId, Text, NodeId, Text)
            -> [(NodeId, Text, NodeId, Text)]
resolveEdge kept fwdAdj (s, sp, d, dp)
  | not (S.member s kept) = []
  | S.member d kept       = [(s, sp, d, dp)]
  | otherwise             =
      [ (s, sp, target, tp)
      | (target, tp) <- bfsToKept kept fwdAdj d ]

-- | BFS from a removed node, following edges through removed nodes,
-- collecting all (kept node, dest port) endpoints reached.
bfsToKept :: S.Set NodeId
          -> M.Map NodeId [(Text, NodeId, Text)]
          -> NodeId
          -> [(NodeId, Text)]
bfsToKept kept fwdAdj start = go S.empty [start]
  where
    go _       []     = []
    go visited (n:ns)
      | S.member n visited = go visited ns
      | otherwise =
          let visited' = S.insert n visited
              succs    = M.findWithDefault [] n fwdAdj
              (found, more) = foldr classify ([], []) succs
          in found ++ go visited' (more ++ ns)
    classify (_sp, d, dp) (found, more)
      | S.member d kept = ((d, dp) : found, more)
      | otherwise       = (found, d : more)

-- | Remove redundant edges: drop a→c if ∃ b such that a→b and b→c both exist.
-- Preserves T/F labeled edges (steer outputs) unconditionally.
-- @skipSet@: nodes to exclude as intermediaries (e.g. NArg nodes whose
-- incoming edges are dashed/cross-activation and represent a different
-- data flow than non-dashed edges).
transitiveReduce :: S.Set NodeId -> [(NodeId, Text, NodeId)] -> [(NodeId, Text, NodeId)]
transitiveReduce skipSet edges =
  let adj = M.fromListWith S.union
              [ (s, S.singleton d) | (s, _, d) <- edges ]
      isSteerLbl x = x == "t" || x == "T" || x == "f" || x == "F"
      -- Edge a→c is redundant if ∃ b with a→b and b→c
      isRedundant (s, sp, d)
        | isSteerLbl sp = False
        | otherwise     =
            let siblings = S.delete d (M.findWithDefault S.empty s adj)
                valid    = S.difference siblings skipSet
            in  any (\b -> S.member d (M.findWithDefault S.empty b adj))
                    (S.toList valid)
  in filter (not . isRedundant) edges

-- | Chain return nodes: use retsnd edges to determine call hierarchy.
-- If retsnd_f feeds into ret_g (where f ≠ g), then f is called from g,
-- so add edge: ret_f → ret_g.  Then remove edges from non-ret sources
-- that skip intermediate rets in the chain.
chainReturns :: M.Map NodeId DNode -> [(NodeId, Text, NodeId)] -> [(NodeId, Text, NodeId)]
chainReturns nodeMap edges =
  let isRet nid = case M.lookup nid nodeMap of
                    Just NRet{} -> True
                    _           -> False
      retName nid = case M.lookup nid nodeMap of
                      Just NRet{nName=n} -> n
                      _                  -> ""
      -- Map function name → ret node ID
      retByName = M.fromList [ (nName dn, nid) | (nid, dn@NRet{}) <- M.toList nodeMap ]
      -- Detect call hierarchy from retsnd→ret edges.
      -- retsnd f#k → ret g  (where f ≠ g)  means g calls f.
      -- So we should add: ret f → ret g.
      retSndCallee nid = case M.lookup nid nodeMap of
                           Just NRetSnd{nName=n} -> takeWhile (/= '#') n
                           _                     -> ""
      -- Collect (ret_f, ret_g) pairs from retsnd→ret edges
      callChains = S.fromList
                     [ (retF, d)
                     | (s, _, d) <- edges
                     , isRet d
                     , let callee = retSndCallee s
                     , not (null callee)
                     , callee /= retName d
                     , Just retF <- [M.lookup callee retByName] ]
      -- Chain edges from the call hierarchy
      chainEdges = [ (f, "", g) | (f, g) <- S.toList callChains ]
      -- Build reverse map: for each ret, which rets chain INTO it?
      retPredecessors = M.fromListWith S.union
                          [ (g, S.singleton f) | (f, g) <- S.toList callChains ]
      -- Remove edges from non-ret sources to ret nodes that have
      -- a predecessor ret in the chain (the source should feed the inner ret,
      -- and the chain connects inner→outer).
      edgesToRemove = S.fromList
                        [ (s, d)
                        | (s, _, d) <- edges
                        , isRet d, not (isRet s)
                        , let preds = M.findWithDefault S.empty d retPredecessors
                        , not (S.null preds) ]
      filtered = [ (s, sp, d) | (s, sp, d) <- edges
                               , not (S.member (s, d) edgesToRemove) ]
  in nub (filtered ++ chainEdges)

-- | Render a node with shape based on its type.
-- progConsts NAddI nodes are relabeled as "const K".
ppCleanNode :: S.Set NodeId -> (NodeId, DNode) -> Text
ppCleanNode progConsts (nid, dn) =
  let label = if S.member nid progConsts
              then case dn of
                     NAddI{iImm=k} -> T.pack ("const " ++ show k)
                     _             -> opSymbol dn
              else opSymbol dn
      attrs = if S.member nid progConsts
              then "shape=box, style=rounded"
              else nodeAttrs dn
  in T.concat [ "  n", T.pack (show nid)
              , " [label=\"", label, "\""
              , ", ", attrs
              , "];" ]

-- | GraphViz attributes (shape, style) per node type.
nodeAttrs :: DNode -> Text
nodeAttrs = \case
  NSteer{}      -> "shape=invtriangle, style=filled, fillcolor=lightyellow"
  NConstI{}     -> "shape=box, style=rounded"
  NConstF{}     -> "shape=box, style=rounded"
  NConstD{}     -> "shape=box, style=rounded"
  NArg{}        -> "shape=box, style=\"rounded,bold\""
  NSuper{}      -> "shape=doubleoctagon, style=filled, fillcolor=lightblue"
  NCallGroup{}  -> "shape=box, style=rounded"
  NCallSnd{}    -> "shape=box, style=rounded"
  NRetSnd{}     -> "shape=box, style=rounded"
  NRet{}        -> "shape=box, style=rounded"
  _             -> "shape=oval"

-- | When deduplicating edges, prefer T/F labels over blank.
pickLabel :: Text -> Text -> Text
pickLabel a b
  | isSteerLabel a = a
  | isSteerLabel b = b
  | otherwise      = a
  where isSteerLabel x = x == "t" || x == "T" || x == "f" || x == "F"

-- | Render a clean edge — label only steer T/F outputs.
ppCleanEdge :: (NodeId, Text, NodeId) -> Text
ppCleanEdge (s, sp, d) =
  let lbl | sp == "t" || sp == "T" = " [label=\"T\", fontsize=10]"
          | sp == "f" || sp == "F" = " [label=\"F\", fontsize=10]"
          | otherwise              = ""
  in T.concat [ "  n", T.pack (show s), " -> n", T.pack (show d), lbl, ";" ]

--------------------------------------------------------------------------------
-- Formal DDG (Dynamic Dataflow Graph — TETC conventions)
--
-- Same semantic content as 'toCleanDot' (callsnd, retsnd, arg, ret, super,
-- ops, constants all visible) but with two visual changes:
--   1. Steering nodes rendered as inverted triangles with T/F outputs
--   2. Return edges (from retsnd, between ret chain) drawn dashed
--------------------------------------------------------------------------------

-- | Edge kind: solid for intra-activation (E), dashed for return (R).
data EdgeKind = Solid | Dashed deriving (Eq, Ord, Show)

-- | Infrastructure for formal DDG — same as 'isInfra' but keeps NSteer.
isInfraFormal :: DNode -> Bool
isInfraFormal = \case
  NValTag{}           -> True
  NTagVal{}           -> True
  NBand{}             -> True
  NCommit{}           -> True
  NStopSpec{}         -> True
  NSteer{}            -> True   -- physical steers removed; virtual T/F triangles added
  NIncTag{}           -> True
  NIncTagI{}          -> True
  NCallGroup{}        -> True
  NCallSnd{}          -> True
  NRetSnd{}           -> True
  NAddI{}             -> True   -- tag offset or constFrom (rescued by semanticConsts)
  NSubI{}             -> True   -- tag offset
  NMulI{}             -> True   -- tag radix (any value)
  NDivI{}             -> True   -- tag radix (any value)
  NEqual{}            -> True
  NGThan{}            -> True
  NGThanI{}           -> True
  NConstI{cInt=k}     -> k == 0 || k == 1
  NSuper{superNum=sn} -> sn <= 3
  _                   -> False

-- | Formal DDG: same content as 'toCleanDot' but with steering triangles
-- and dashed return edges.  When a function name is given, only that
-- function's subgraph is shown.
toFormalDot :: Maybe String -> DGraph DNode -> TL.Text
toFormalDot mFn g =
  let nodes    = M.toList (dgNodes g)
      edges    = [ (s, T.pack sp, d, T.pack dp) | (s,sp,d,dp) <- dgEdges g ]
      nodeMap  = dgNodes g

      -- ── Zeroing subs (same as toCleanDot) ──
      inEdges  = M.fromListWith (++)
                   [ (d, [s]) | (s, _sp, d, _dp) <- edges ]
      zeroSubs = S.fromList
                   [ nid | (nid, NSub{}) <- nodes
                         , let srcs = M.findWithDefault [] nid inEdges
                         , not (null srcs)
                         , S.size (S.fromList srcs) == 1 ]

      -- ── Program constants ──
      progConsts = S.fromList
                     [ nid | (nid, NAddI{}) <- nodes
                           , let srcs = M.findWithDefault [] nid inEdges
                           , any (`S.member` zeroSubs) srcs ]
      fwdAll   = M.fromListWith (++) [ (s, [d]) | (s,_,d,_) <- dgEdges g ]

      -- ── Infra + conditional (for joinAdds) ──
      infraOrCond = S.fromList [ nid | (nid, dn) <- nodes
                                     , isInfraFormal dn || isConditional dn ]
      allInfra    = S.union infraOrCond zeroSubs

      -- NAdd is semantic (e.g. fib(n-1)+fib(n-2)) when ALL its inputs are
      -- NValTag nodes (value pipeline carriers from retsnd chains).
      -- Otherwise (inputs from comparisons, guards, zeroing subs) it's a
      -- condition-combiner or MUX join → infrastructure.
      isValTagSrc nid = case M.lookup nid nodeMap of
                          Just NValTag{} -> True
                          _              -> False
      joinAdds = S.fromList
                   [ nid | (nid, NAdd{}) <- nodes
                         , let srcs = M.findWithDefault [] nid inEdges
                         , not (null srcs)
                         , all (`S.member` allInfra) srcs
                         , not (all isValTagSrc srcs) ]

      -- ── Kept set ──
      keptBase = S.fromList [ nid | (nid, dn) <- nodes
                                  , not (isInfraFormal dn)
                                  , not (S.member nid zeroSubs)
                                  , not (S.member nid joinAdds) ]
      semanticConsts = S.fromList
                         [ nid | nid <- S.toList progConsts
                               , let succs = M.findWithDefault [] nid fwdAll
                               , any (`S.member` keptBase) succs ]
      kept     = S.union keptBase semanticConsts

      -- ── Edge resolution (reuse toCleanDot helpers) ──
      fwdAdj   = M.fromListWith (++)
                   [ (s, [(sp, d, dp)]) | (s, sp, d, dp) <- edges
                                        , not (S.member s kept) ]
      cleanEdges = nub $ concatMap (resolveEdge kept fwdAdj) edges
      -- Dedup: preserve distinct T/F steer labels as separate edges
      isSteerLbl x = x == "t" || x == "T" || x == "f" || x == "F"
      deduped    = M.toList $ M.fromListWith pickLabel
                     [ ( (s, d, if isSteerLbl sp then sp else ""), sp )
                     | (s, sp, d, _dp) <- cleanEdges, s /= d
                     , not (S.member d semanticConsts) ]
      finalEdges0 = [ (s, sp, d) | ((s, d, _), sp) <- deduped ]

      -- ── Function assignment: backward-trace to NArg to find function ──
      bwdMap0 = M.fromListWith (++) [ (d, [s]) | (s,_,d) <- finalEdges0 ]
      findFn nid = go S.empty [nid]
        where
          go _ [] = Nothing
          go vis (n:ns)
            | S.member n vis = go vis ns
            | otherwise = case M.lookup n nodeMap of
                Just NArg{nName=nm} -> Just (takeWhile (/= '#') nm)
                Just NRet{nName=nm} -> Just nm
                _ -> go (S.insert n vis)
                         (M.findWithDefault [] n bwdMap0 ++ ns)
      nodeFn = M.fromList [ (nid, fn) | (nid, _) <- nodes
                                       , Just fn <- [findFn nid] ]

      -- ── Label comparison edges T/F (scoped to same function) ──
      lkNode nid = M.findWithDefault (NConstI "" 0) nid nodeMap
      isRetNode NRet{} = True
      isRetNode _      = False
      labeled = map labelCond finalEdges0
      labelCond (s, sp, d) =
        case (isConditional (lkNode s), M.lookup s nodeFn, M.lookup d nodeFn) of
          (True, Just fs, Just fd) | fs == fd ->
            if isRetNode (lkNode d) then (s, "T", d) else (s, "F", d)
          _ -> (s, sp, d)

      -- Skip NArg nodes as intermediaries: edges through NArg are dashed
      -- (cross-activation) and don't make non-dashed edges redundant.
      argSkip    = S.fromList [ nid | (nid, NArg{}) <- nodes ]
      reduced    = transitiveReduce argSkip labeled
      chained    = chainReturns nodeMap reduced
      finalEdges1 = transitiveReduce argSkip chained

      -- ── Insert virtual steer triangle per comparison node ──
      -- comparison → target(T/F) becomes comparison → steer, steer → target(T/F)
      compNodes  = [ nid | (nid, dn) <- nodes, isConditional dn
                         , S.member nid kept ]
      maxNid     = 1 + maximum (0 : [ nid | (nid, _) <- nodes ])
      compToVirt = M.fromList (zip compNodes [maxNid ..])
      insertSteer (s, sp, d)
        | Just vid <- M.lookup s compToVirt
        , sp == "T" || sp == "F"  = [(s, "", vid), (vid, sp, d)]
        | otherwise               = [(s, sp, d)]
      steerEdges = nub $ concatMap insertSteer finalEdges1
      virtNodes  = [ (vid, NSteer "") | (_, vid) <- M.toList compToVirt
                   , any (\(s,_,_) -> s == vid) steerEdges ]

      -- ── Base-case value: trace physical steers to find T-branch constants ──
      -- For if-then-else, the "then" value (base case) feeds a physical steer
      -- on port 1.  Trace backward from port 1 through infra to find the
      -- program constant, and from port 0 backward to the comparison node.
      edgesByDP = M.fromListWith (++)
          [ ((d, dp), [s]) | (s, _sp, d, dp) <- edges ]
      bwdRaw    = M.fromListWith (++) [ (d, [s]) | (s, _, d, _) <- edges ]
      steerTTarget = M.fromList
          [ (s, d) | (s, sp, d, _dp) <- edges
          , case M.lookup s nodeMap of { Just NSteer{} -> True; _ -> False }
          , sp == "t" || sp == "T" ]
      isDeadTarget d = case M.lookup d nodeMap of
          Just NStopSpec{} -> True; Just NCommit{} -> True; _ -> False
      traceComp seeds = go S.empty seeds
        where
          go _ []     = []
          go vis (n:ns)
            | S.member n vis = go vis ns
            | isConditional (lkNode n) = [n]
            | isInfraFormal (lkNode n) || S.member n zeroSubs =
                go (S.insert n vis) (M.findWithDefault [] n bwdRaw ++ ns)
            | otherwise = go (S.insert n vis) ns
      traceVal seeds = go S.empty seeds
        where
          go _ []     = Nothing
          go vis (n:ns)
            | S.member n vis = go vis ns
            | otherwise =
                let vis' = S.insert n vis
                    preds = M.findWithDefault [] n bwdRaw
                in if S.member n progConsts
                   then case M.lookup n nodeMap of
                          Just NAddI{iImm=k} -> Just k
                          _                  -> go vis' ns
                   else case M.lookup n nodeMap of
                          Just NConstI{cInt=k}  -> Just k
                          Just dn | isInfraFormal dn || S.member n zeroSubs
                                    -> go vis' (preds ++ ns)
                          _         -> go vis' ns
      steerTVals = M.fromList
          [ (comp, val)
          | (sid, NSteer{}) <- nodes
          , Just tgt <- [M.lookup sid steerTTarget]
          , not (isDeadTarget tgt)
          , let p0 = M.findWithDefault [] (sid, "0") edgesByDP
                p1 = M.findWithDefault [] (sid, "1") edgesByDP
          , comp <- traceComp p0
          , Just val <- [traceVal p1]
          ]
      virtTVals = M.fromList
          [ (vid, val)
          | (comp, vid) <- M.toList compToVirt
          , Just val <- [M.lookup comp steerTVals]
          ]
      nextVirtId   = maxNid + length compNodes
      baseCaseMap  = M.fromList $ zip (M.keys virtTVals) [nextVirtId ..]
      baseCaseNds  = [ (cid, NConstI "" val)
                     | (vid, cid) <- M.toList baseCaseMap
                     , Just val <- [M.lookup vid virtTVals] ]
      insertBase (s, sp, d)
        | sp == "T", Just cid <- M.lookup s baseCaseMap
        = [(s, "T", cid), (cid, "", d)]
        | otherwise = [(s, sp, d)]
      finalEdges = nub $ concatMap insertBase steerEdges

      -- ── Dashed edges: cross-activation (recursive call/return) ──
      -- 1. Edges INTO NArg = recursive call argument
      -- 2. NRet → NRet = return chain
      isArgNode' nid = case M.lookup nid nodeMap of
                         Just NArg{} -> True
                         _           -> False
      isDashed (_, _, d)
        | isArgNode' d = True
      isDashed (s, _, d) =
        case (M.lookup s nodeMap, M.lookup d nodeMap) of
          (Just NRet{}, Just NRet{}) -> True
          _                          -> False

      -- ── Caller constants: NConstI/F/D whose original-graph successors ──
      -- are ALL infra (e.g. const 10 → IT nodes from main's call setup).
      -- These are call-site values from the caller, not function internals.
      callerConsts = S.fromList
        [ nid | (nid, dn) <- nodes
              , case dn of NConstI{} -> True; NConstF{} -> True
                           NConstD{} -> True; _         -> False
              , S.member nid kept
              , let succs = M.findWithDefault [] nid fwdAll
              , not (null succs)
              , all (\d -> case M.lookup d nodeMap of
                             Just dn' -> isInfraFormal dn'
                             Nothing  -> True) succs ]

      -- ── Only keep nodes in at least one edge ──
      edgeNodes  = S.fromList $ concatMap (\(s,_,d) -> [s,d]) finalEdges
      cleanNodes = sortOn fst
                     ([ (nid, dn) | (nid, dn) <- nodes
                                  , S.member nid kept
                                  , S.member nid edgeNodes ]
                      ++ virtNodes
                      ++ baseCaseNds)

      -- ── Function scope filter ──
      (scopedNodes, scopedEdges) = case mFn of
        -- Explicit function name: backward BFS from that function's NRet
        Just fn ->
          let fwd = M.fromListWith (++) [ (s, [d]) | (s,_,d) <- finalEdges ]
              bwd = M.fromListWith (++) [ (d, [s]) | (s,_,d) <- finalEdges ]
              bfsFrom adj seed = go S.empty (S.toList seed)
                where
                  go vis []     = vis
                  go vis (n:ns)
                    | S.member n vis = go vis ns
                    | otherwise      = go (S.insert n vis)
                                          (M.findWithDefault [] n adj ++ ns)
              retSeed = S.fromList
                          [ nid | (nid, dn) <- cleanNodes
                                , case dn of NRet{} -> nName dn == fn
                                             _      -> False ]
          in if S.null retSeed
             then (cleanNodes, finalEdges)
             else
               let bwdReach = bfsFrom bwd retSeed
                   isArgNode nid = case M.lookup nid nodeMap of
                                     Just NArg{} -> True
                                     _           -> False
                   isConstNode nid = case M.lookup nid nodeMap of
                                       Just NConstI{} -> True
                                       Just NConstF{} -> True
                                       Just NConstD{} -> True
                                       _  -> S.member nid progConsts
                   argOnlyPreds = S.fromList
                                    [ p | n <- S.toList bwdReach, isArgNode n
                                        , p <- M.findWithDefault [] n bwd
                                        , isConstNode p
                                        , all isArgNode
                                              (M.findWithDefault [] p fwd) ]
                   scope    = S.difference bwdReach
                                (S.union argOnlyPreds callerConsts)
                   sNodes = filter (\(nid,_) -> S.member nid scope) cleanNodes
                   sEdges = filter (\(s,_,d) -> S.member s scope
                                             && S.member d scope) finalEdges
               in (sNodes, sEdges)

        -- No function name: exclude boilerplate (main, print_*) by node name
        -- and call-site constants that only feed NArg nodes.
        Nothing ->
          let isBoilerFn fn = fn == "main" || "print_" `isPrefixOf` fn
              isBoilerNode nid = case M.lookup nid nodeMap of
                Just NArg{nName=nm} -> isBoilerFn (takeWhile (/= '#') nm)
                Just NRet{nName=nm} -> isBoilerFn nm
                _                   -> False
              fwd = M.fromListWith (++) [ (s, [d]) | (s,_,d) <- finalEdges ]
              bwd = M.fromListWith (++) [ (d, [s]) | (s,_,d) <- finalEdges ]
              isArgNode nid = case M.lookup nid nodeMap of
                                Just NArg{} -> True
                                _           -> False
              isConstNode nid = case M.lookup nid nodeMap of
                                  Just NConstI{} -> True
                                  Just NConstF{} -> True
                                  Just NConstD{} -> True
                                  _  -> S.member nid progConsts
              argOnlyPreds = S.fromList
                               [ p | (nid, _) <- cleanNodes, isArgNode nid
                                   , p <- M.findWithDefault [] nid bwd
                                   , isConstNode p
                                   , all isArgNode
                                         (M.findWithDefault [] p fwd) ]
              exclude = S.unions
                          [ S.fromList [ nid | (nid, _) <- cleanNodes
                                             , isBoilerNode nid ]
                          , argOnlyPreds
                          , callerConsts ]
              sEdges = filter (\(s,_,d) -> not (S.member s exclude)
                                        && not (S.member d exclude)) finalEdges
              eNodes = S.fromList $ concatMap (\(s,_,d) -> [s,d]) sEdges
              sNodes = filter (\(nid,_) -> S.member nid eNodes) cleanNodes
          in (sNodes, sEdges)

      virtSteerSet = S.fromList [ vid | (vid, _) <- virtNodes ]

  in TL.fromStrict . T.unlines $
       [ "digraph DDG {"
       , "  rankdir=TB;"
       , "  node [fontsize=12];"
       , "  edge [fontsize=10];"
       ]
       ++ map (ppFormalNode progConsts) scopedNodes
       ++ map (\e -> ppFormalEdge virtSteerSet e (isDashed e))
              (sortOn (\(s,_,d) -> (d,s)) scopedEdges)
       ++ [ "}" ]

-- | Node shape for formal DDG: circle by default, triangle for steer.
formalNodeAttrs :: DNode -> Text
formalNodeAttrs = \case
  NSteer{} -> "shape=triangle, fontsize=10"
  NSuper{} -> "shape=doubleoctagon"
  _        -> "shape=circle"

-- | Render a formal DDG node. Steers get empty label (triangle shape suffices).
ppFormalNode :: S.Set NodeId -> (NodeId, DNode) -> Text
ppFormalNode progConsts (nid, dn) =
  let label = if S.member nid progConsts
              then case dn of
                     NAddI{iImm=k} -> T.pack (show k)
                     _             -> formalOpSymbol dn
              else formalOpSymbol dn
      attrs = formalNodeAttrs dn
  in T.concat [ "  n", T.pack (show nid)
              , " [label=\"", label, "\""
              , ", ", attrs
              , "];" ]

-- | Render a formal DDG edge with optional dashed style.
-- Steer edges use ports: input enters :n (top), F exits :sw (left), T exits :se (right).
ppFormalEdge :: S.Set NodeId -> (NodeId, Text, NodeId) -> Bool -> Text
ppFormalEdge steers (s, sp, d) dashed =
  let isSrcSteer = S.member s steers
      isDstSteer = S.member d steers
      srcPort | isSrcSteer, sp == "T" || sp == "t" = ":sw"
              | isSrcSteer, sp == "F" || sp == "f" = ":se"
              | otherwise = ""
      dstPort | isDstSteer = ":n"
              | otherwise  = ""
      attrs = if dashed then " [style=dashed]" else ""
  in T.concat [ "  n", T.pack (show s), srcPort
              , " -> n", T.pack (show d), dstPort
              , attrs, ";" ]

-- | Label for formal DDG: clean mathematical notation, no TALM mnemonics.
formalOpSymbol :: DNode -> Text
formalOpSymbol = \case
  NSteer{}    -> "T     F"
  NArg{..}    -> T.pack nName
  NRet{}      -> "Return\\noutput"
  NConstI{..} -> T.pack (show cInt)
  NConstF{..} -> T.pack (show cFloat)
  NConstD{..} -> T.pack (show cDouble)
  NAdd{}      -> "+"
  NSub{}      -> "-"
  NMul{}      -> "*"
  NDiv{}      -> "/"
  NFAdd{}     -> "f+"
  NFSub{}     -> "f-"
  NFMul{}     -> "f*"
  NFDiv{}     -> "f/"
  NDAdd{}     -> "d+"
  NLThan{}    -> "<"
  NGThan{}    -> ">"
  NEqual{}    -> "=="
  NLThanI{..} -> T.pack ("<" ++ show iImm)
  NGThanI{..} -> T.pack (">" ++ show iImm)
  NAddI{..}   -> T.pack ("+" ++ show iImm)
  NSubI{..}   -> T.pack ("-" ++ show iImm)
  NMulI{..}   -> T.pack ("*" ++ show iImm)
  NFMulI{..}  -> T.pack ("*" ++ show fImm)
  NDivI{..}   -> T.pack ("/" ++ show iImm)
  NSuper{..}  -> T.pack nName
  _           -> ""

--------------------------------------------------------------------------------
-- Shared helpers
--------------------------------------------------------------------------------

sortedNodes :: DGraph a -> [(NodeId, a)]
sortedNodes g = sortOn fst (M.toList (dgNodes g))

sortedEdges :: DGraph n -> [(NodeId, Text, NodeId, Text)]
sortedEdges g =
  let es = [ (s, T.pack sp, d, T.pack dp) | (s,sp,d,dp) <- dgEdges g ]
  in sortOn (\(s,_sp,d,dp) -> (d, toInt dp, s)) es

toInt :: Text -> Int
toInt t | T.all isDigit t && not (T.null t) = read (T.unpack t)
toInt _ = 0

ppNode :: DGraph DNode -> (NodeId, DNode) -> Text
ppNode _ (nid, dn) =
  T.concat [ "  n", T.pack (show nid), " [label=\"", opSymbol dn, "\"];" ]

-- | Mnemonic for each 'DNode' variant.
opSymbol :: DNode -> Text
opSymbol = \case
  NConstI{..}   -> T.pack ("const "  ++ show cInt)
  NConstF{..}   -> T.pack ("fconst " ++ show cFloat)
  NConstD{..}   -> T.pack ("dconst " ++ show cDouble)
  NAdd{}        -> "+"
  NSub{}        -> "-"
  NMul{}        -> "*"
  NDiv{}        -> "/"
  NFAdd{}       -> "f+"
  NFSub{}       -> "f-"
  NFMul{}       -> "f*"
  NFDiv{}       -> "f/"
  NDAdd{}       -> "d+"
  NBand{}       -> "band"
  NSteer{}      -> "steer"
  NLThan{}      -> "<"
  NGThan{}      -> ">"
  NEqual{}      -> "=="
  NLThanI{..}   -> T.pack ("<i "  ++ show iImm)
  NGThanI{..}   -> T.pack (">i "  ++ show iImm)
  NAddI{..}     -> T.pack ("+i "  ++ show iImm)
  NSubI{..}     -> T.pack ("-i "  ++ show iImm)
  NMulI{..}     -> T.pack ("*i "  ++ show iImm)
  NFMulI{..}    -> T.pack ("*fi " ++ show fImm)
  NDivI{..}     -> T.pack ("/i "  ++ show iImm)
  NCallGroup{..}-> T.pack ("callgroup " <> nName)
  NCallSnd{..}  -> T.pack ("callsnd "   <> nName)
  NRetSnd{..}   -> T.pack ("retsnd "    <> nName)
  NRet{..}      -> T.pack ("ret "       <> nName)
  NTagVal{}     -> "tagval"
  NValTag{}     -> "valtag"
  NIncTag{}     -> "IT"
  NIncTagI{..}  -> T.pack ("IT " ++ show iImm)
  NCpHToDev{}   -> "cphtodev"
  NCpDevToH{}   -> "cpdevtoh"
  NCommit{}     -> "commit"
  NStopSpec{}   -> "stopspec"
  NArg{..}      -> T.pack ("arg "       <> nName)
  NSuper{}      -> "super"

ppEdge :: (NodeId, Text, NodeId, Text) -> Text
ppEdge (s, sp, d, dp) =
  T.concat
    [ "  n", T.pack (show s), " -> n", T.pack (show d)
    , " [label=\"(", if T.null sp then "0" else T.toUpper sp, ",", dp, ")\", fontsize=10];"
    ]
