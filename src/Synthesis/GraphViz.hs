{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}
-- | Flow.GraphViz – render a list of `Inst` into the same DOT syntax that
--   Couillard’s original Python `GraphVizVisitor` outputs.
--   Node labels, shapes and (head|tail)port conventions are preserved so the
--   produced file is accepted by Trebuchet unchanged.
module Synthesis.GraphViz (render, saveDot) where

import           Synthesis.Instruction
import           Data.Text.Lazy.Builder          (Builder)
import qualified Data.Text.Lazy.Builder          as B
import qualified Data.Text.Lazy                 as TL
import qualified Data.Text.Lazy.IO              as TLIO
import           Data.Maybe                      (fromMaybe)

-------------------------------------------------------------------------------
-- Public helpers -------------------------------------------------------------
-------------------------------------------------------------------------------

-- | Produce DOT as lazy Text.
render :: [Inst] -> TL.Text
render insts = B.toLazyText $
       B.fromString "digraph G{\n"
    <> foldMap nodeDecl insts
    <> foldMap (edgeDecl insts) insts
    <> B.fromString "}\n"

-- | Save DOT to a file.
saveDot :: FilePath -> [Inst] -> IO ()
saveDot fp = TLIO.writeFile fp . render

-------------------------------------------------------------------------------
-- Nodes ----------------------------------------------------------------------
-------------------------------------------------------------------------------

nodeDecl :: Inst -> Builder
nodeDecl = \case
  InstConst{..} ->
   let lbl = if litType=="float"
            then "fconst #"<>showB (fromIntegral litVal / 100 :: Double)
            else "const #" <> showB litVal
   in  box nodeId lbl
  InstBinop{..}  -> box nodeId (B.fromString binOp)
  InstBinopI{..} -> box nodeId (B.fromString (binOp <> immStr immedVal))
  InstSteer{..}  -> B.fromString $ "  " ++ sid nodeId ++ " [shape=triangle style=solid label=\"T   F\"];\n"
  InstIncTag{..} -> circle nodeId "IT"
  InstSuper{..}  -> rect   nodeId ("S" <> showB superNum)
  InstPar{..}    -> rect   nodeId ("S" <> showB parNum)
  _              -> mempty -- call/ret not drawn in original visitor
 where
   immStr n | n >= 0    = show n
            | otherwise = "(" <> show n <> ")"

-- Helpers for node shapes -----------------------------------------------------
box, rect, circle :: NodeId -> Builder -> Builder
box    = genericNode "box" "rounded"
rect   = genericNode "rectangle" "solid"
circle = genericNode "circle" ""

genericNode :: String -> String -> NodeId -> Builder -> Builder
genericNode shape sty nid lbl =
  let lab = TL.unpack (B.toLazyText lbl)
  in  B.fromString $ concat
        [ "  ", sid nid, " [shape=", shape
        , if null sty then "" else " style="<>sty
        , " label=\"", lab, "\"];\n" ]

sid :: NodeId -> String
sid (NodeId i) = "n" <> show i

showB :: Show a => a -> Builder
showB = B.fromString . show

-------------------------------------------------------------------------------
-- Edges ----------------------------------------------------------------------
-------------------------------------------------------------------------------

edgeDecl :: [Inst] -> Inst -> Builder
edgeDecl allInsts inst = mconcat (edgesFor inst)
  where edgesFor = edgesFrom inst

-- produce edges for a single instruction -------------------------------------
edgesFrom :: Inst -> Inst -> [Builder]
edgesFrom origin = \case
  InstConst{}   -> []
  InstBinop{..} ->  zipWith (edge "nw") leftSrc  (repeat Nothing)
                ++ zipWith (edge "ne") rightSrc (repeat Nothing)
  InstBinopI{..}->  map (\s -> edge "n" s Nothing) uniSrc
  InstSteer{..} ->  map edgeDefault steerInp
                ++ map (\c -> edge "n" c Nothing) steerExpr
  InstIncTag{..}->  map edgeDefault tagInp
  InstSuper{..} ->  concatMap (map edgeDefault) superInp
  InstPar{..}   ->  concatMap (map edgeDefault) parInp
  _             ->  []
  where
    edge hp sig = printEdge sig origin (Just hp)   -- tailport→headport swap
    edgeDefault sig = printEdge sig origin Nothing Nothing

-------------------------------------------------------------------------------
-- Edge printer ----------------------------------------------------------------
-------------------------------------------------------------------------------

printEdge :: Signal       -- ^ source
          -> Inst         -- ^ destination
          -> Maybe String -- ^ tailport param (→ headport)
          -> Maybe String -- ^ headport param (→ tailport)
          -> Builder
printEdge src dst mTail mHead =
  let (srcNode, defaultTail) = case src of
        SigInstPort nid _ _ -> (sid nid, "s")
        SigSteerPort nid br -> (sid nid, case br of { T -> "sw"; F -> "se" })
        SigReturnPort{}     -> ("return", "s")
      tailP = fromMaybe defaultTail mHead   -- swapped
      headP = fromMaybe "n" mTail           -- swapped
      dstNode = sid (nodeId dst)
  in  B.fromString $ concat
        [ "  ", srcNode, " -> ", dstNode
        , " [tailport=", tailP, ", headport=", headP, "];\n" ]
