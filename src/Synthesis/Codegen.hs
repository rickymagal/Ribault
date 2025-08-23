{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Synthesis.Codegen
  ( assemble
  ) where

import qualified Data.Map  as M
import qualified Data.Set  as S
import           Data.List (intercalate, sortOn, nub)
import           Data.Maybe (fromMaybe)
import           Numeric   (showFFloat)
import qualified Data.Text as T

import           Types (DGraph(..), NodeId)
import           Node  (DNode(..))

-- API -------------------------------------------------------------------

-- Converte o grafo de Inst para assembly TALM (Text estrito).
assemble :: DGraph DNode -> T.Text
assemble g =
  let nodes       = sortOn fst (M.toList (dgNodes g))
      edges       = dgEdges g
      nmap        = M.fromList nodes
      inMap       = buildInputs edges
      superDecls  = dedupSupers nodes
      allRetChans = dedupRetChans nodes
      hdr         = map ppSuperDecl superDecls
      body        = concatMap (\(nid, dn) -> emitNode nmap inMap allRetChans nid dn) nodes
  in T.unlines (hdr ++ body)

-- Mapa (destNode, destPort) -> [(srcNode, srcPort)]
buildInputs :: [(NodeId, String, NodeId, String)]
            -> M.Map (NodeId, String) [(NodeId, String)]
buildInputs es =
  let step m (s,sp,d,dp) = M.insertWith (++) (d,dp) [(s,sp)] m
  in foldl step M.empty es

-- SUPERs declaradas (sem repetição)
dedupSupers :: [(NodeId, DNode)] -> [(String, Int, Bool)]
dedupSupers = go S.empty []
  where
    go _   acc []                       = reverse acc
    go seen acc ((_, NSuper{..}) : xs) =
      let key = superKey nName superOuts superSpec
      in if key `S.member` seen
           then go seen acc xs
           else go (S.insert key seen) ((nName, superOuts, superSpec):acc) xs
    go seen acc (_:xs)                  = go seen acc xs
    superKey nm outs sp = nm ++ "|" ++ show outs ++ "|" ++ show sp

ppSuperDecl :: (String, Int, Bool) -> T.Text
ppSuperDecl (nm, outs, spec) =
  T.pack $ "superinst(\"" ++ nm ++ "\", 0, " ++ show outs ++ ", " ++ mapBool spec ++ ")"
  where
    mapBool True  = "True"
    mapBool False = "False"

-- Emissão de 0..N linhas para um nó
emitNode :: M.Map NodeId DNode
         -> M.Map (NodeId, String) [(NodeId, String)]
         -> [T.Text]  -- ^ canais usados em retsnd: ["foo[0]","bar[0]"]
         -> NodeId
         -> DNode
         -> [T.Text]
emitNode nmap inMap allRetChans nid dn =
  case dn of
    -- Constantes
    NConstI{..} -> one $ T.pack ("const "  ++ outVar nid ++ ", " ++ show cInt)
    NConstF{..} -> one $ T.pack ("fconst " ++ outVar nid ++ ", " ++ showF 6 cFloat)
    NConstD{..} -> one $ T.pack ("dconst " ++ outVar nid ++ ", " ++ showF 6 cDouble)

    -- Binárias
    NAdd{}      -> one $ bin2 "add"
    NSub{}      -> one $ bin2 "sub"
    NMul{}      -> one $ bin2 "mul"
    NDiv{}      -> let [a,b] = insAB
                       (o0,o1) = (outVar0 nid, outVar1 nid)
                   in [T.pack ("div " ++ o0 ++ "," ++ o1 ++ ", " ++ a ++ ", " ++ b)]
    NBand{}     -> one $ bin2 "band"
    NFAdd{}     -> one $ bin2 "fadd"
    NDAdd{}     -> one $ bin2 "dadd"

    -- Imediatas
    NAddI{..}   -> one $ T.pack ("addi "  ++ outVar nid ++ ", " ++ arg0 ++ ", " ++ show iImm)
    NSubI{..}   -> one $ T.pack ("subi "  ++ outVar nid ++ ", " ++ arg0 ++ ", " ++ show iImm)
    NMulI{..}   -> one $ T.pack ("muli "  ++ outVar nid ++ ", " ++ arg0 ++ ", " ++ show iImm)
    NFMulI{..}  -> one $ T.pack ("fmuli " ++ outVar nid ++ ", " ++ arg0 ++ ", " ++ showF 6 fImm)
    NDivI{..}   -> let (o0,o1) = (outVarK nid 0, outVarK nid 1)
                   in [T.pack ("divi " ++ o0 ++ "," ++ o1 ++ ", " ++ arg0 ++ ", " ++ show iImm)]

    -- Comparações / steer
    NLThan{}    -> one $ bin2 "lthan"
    NGThan{}    -> one $ bin2 "gthan"
    NEqual{}    -> one $ bin2 "equal"
    NLThanI{..} -> one $ T.pack ("lthani " ++ outVar nid ++ ", " ++ arg0 ++ ", " ++ show iImm)
    NGThanI{..} -> one $ T.pack ("gthani " ++ outVar nid ++ ", " ++ arg0 ++ ", " ++ show iImm)

    NSteer{}    -> one $ T.pack ("steer " ++ steerName nid ++ ", " ++ cond1 ++ ", " ++ arg0)

    -- Chamadas TALM
    NCallGroup{..} ->
      [ T.pack $ "callgroup(\"" ++ tagName nid ++ "\",\"" ++ nName ++ "\")" ]
    NCallSnd{..} ->
      let (fname, idx) = splitHash nName   -- nName = "f#i"
          tagSym       = tagOf (argOf "1")
      in [ T.pack $ "callsnd " ++ fname ++ "[" ++ show idx ++ "], " ++ arg0 ++ ", " ++ tagSym ]
    NRetSnd{..}  ->
      let fname = nName
          tagSym = tagOf (argOf "1")
      in [ T.pack $ "retsnd " ++ fname ++ "[0], " ++ arg0 ++ ", " ++ tagSym ]
    NRet{..}     ->
      let xs = if null allRetChans
                 then nName ++ "[0]"
                 else "[" ++ intercalate "," (map T.unpack allRetChans) ++ "]"
      in [ T.pack $ "ret " ++ nName ++ ", " ++ arg0 ++ ", " ++ xs ]

    -- Conversores / DMA
    NTagVal{}    -> one $ un1 "tagval"
    NValTag{}    -> one $ un1 "valtag"
    NCpHToDev{}  -> one $ un1 "cphtodev"
    NCpDevToH{}  -> one $ un1 "cpdevtoh"

    -- Spec
    NCommit{}    -> let [a] = insA
                        (o0,o1) = (outVar0 nid, outVar1 nid)
                    in [ T.pack ("commit " ++ o0 ++ "," ++ o1 ++ ", " ++ a) ]
    NStopSpec{}  -> let [a] = insA
                        (o0,o1) = (outVar0 nid, outVar1 nid)
                    in [ T.pack ("stopspec " ++ o0 ++ "," ++ o1 ++ ", " ++ a) ]

    -- Argumento formal: não emite linha; nome é resolvido pelos leitores.
    NArg{}       -> []

    -- SUPER (declaração sai no cabeçalho; aqui é a invocação)
    NSuper{..}   ->
      let outs = [ outVarK nid k | k <- [0 .. max 0 (superOuts-1)] ]
          -- entradas ordenadas "0","1","2",...
          inps = let gather k acc =
                         case M.lookup (nid, show k) inMap of
                           Nothing -> reverse acc
                           Just xs -> gather (k+1) (opText xs : acc)
                 in gather 0 []
          line = nName ++ " " ++ intercalate ", " (outs ++ inps)
      in [ T.pack line ]
  where
    -- entradas comuns
    insA   = [opText (argOf "0")]
    insAB  = [opText (argOf "0"), opText (argOf "1")]
    -- operandos
    arg0   = opText (argOf "0")
    cond1  = opText (argOf "1")
    -- ajudantes de impressão
    one t  = [t]
    bin2 op = T.pack (op ++ " " ++ outVar nid ++ ", " ++ opText (argOf "0") ++ ", " ++ opText (argOf "1"))
    un1  op = T.pack (op ++ " " ++ outVar nid ++ ", " ++ opText (argOf "0"))
    -- entradas do nó/porta
    argOf dp = fromMaybe [] (M.lookup (nid, dp) inMap)

    -- um operando: simples "x" ou lista "[a,b,c]"
    opText []  = "0"
    opText [s] = refOf s
    opText ss  = "[" ++ intercalate "," (map refOf ss) ++ "]"

    -- como referenciar a saída (sid, sp)
    refOf (sid, sp) =
      case M.lookup sid nmap of
        Just NSteer{}     -> steerName sid ++ "." ++ sp
        Just NDiv{}       -> if sp == "0" then outVar0 sid else outVar1 sid
        Just NDivI{}      -> if sp == "0" then outVarK sid 0 else outVarK sid 1
        Just NCommit{}    -> if sp == "0" then outVar0 sid else outVar1 sid
        Just NStopSpec{}  -> if sp == "0" then outVar0 sid else outVar1 sid
        Just NSuper{}     ->
          case reads sp of
            [(i,_)]  -> outVarK sid i     -- <- removida a anotação ::Int
            _        -> outVar sid
        Just NCallGroup{} -> tagName sid
        Just NArg{..}     -> argFormal nName
        _                 -> outVar sid

    tagOf []      = tagName (-1)
    tagOf (s:_)   = case M.lookup (fst s) nmap of
                      Just NCallGroup{} -> tagName (fst s)
                      _                 -> outVar (fst s)

-- Nomes -----------------------------------------------------------------

outVar :: NodeId -> String
outVar nid = "n" ++ show nid

outVar0 :: NodeId -> String
outVar0 nid = "n" ++ show nid ++ "a"

outVar1 :: NodeId -> String
outVar1 nid = "n" ++ show nid ++ "b"

-- para nós com >1 saídas (SUPER, DivI genérico, etc.)
outVarK :: NodeId -> Int -> String
outVarK nid k = "n" ++ show nid ++ "_" ++ show k

steerName :: NodeId -> String
steerName nid = "s" ++ show nid

tagName :: NodeId -> String
tagName nid = "c" ++ show nid

-- "foo#0" -> "foo.call1" (usado para formais NArg)
argFormal :: String -> String
argFormal nm = case break (=='#') nm of
  (fname, '#':ix) ->
    case reads ix of
      [(i, _)] -> fname ++ ".call" ++ show (i+1)
      _        -> fname ++ ".call?"
  _ -> nm ++ ".call?"

-- "foo#7" -> ("foo", 7) (usado em callsnd)
splitHash :: String -> (String, Int)
splitHash nm = case break (=='#') nm of
  (fname, '#':ix) -> case reads ix of
                       [(i,_)] -> (fname, i)
                       _       -> (fname, 0)
  _                -> (nm, 0)

-- Float/Double pretty
showF :: Real a => Int -> a -> String
showF n x = showFFloat (Just n) (realToFrac x :: Double) ""

-- Lista deduplicada de canais passados em retsnd (para o 'ret ... , [...]')
dedupRetChans :: [(NodeId, DNode)] -> [T.Text]
dedupRetChans xs =
  let names = [ T.pack (nm ++ "[0]") | (_, NRetSnd{ nName = nm }) <- xs ]
  in nub names
