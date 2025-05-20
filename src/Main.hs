{-# LANGUAGE LambdaCase, OverloadedStrings #-}

-- | Entry point and DOT graph generator for the LambdaFlow compiler frontend.
-- Parses input, performs semantic analysis, and outputs an AST in DOT format.
module Main where

import System.Environment (getArgs)
import System.IO          (readFile, getContents)
import System.Exit        (exitFailure)
import Data.Text.Lazy     (Text)
import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as T

import Lexer   (alexScanTokens)
import Parser  (parse)
import Syntax  (Program(..), Decl(..), Expr(..), Pattern(..))
import Semantic (checkAll)

-- | Program entry point. Reads from a file or stdin, parses,
-- checks semantics, and prints the AST in DOT or errors.
main :: IO ()
main = do
  args  <- getArgs
  input <- case args of
    [filename] -> readFile filename    -- ^ Read from file if filename given
    []         -> getContents          -- ^ Otherwise read from stdin
    _          -> putStrLn "Uso: lexer [arquivo]" >> exitFailure

  -- | Parse the input into an AST.
  let ast = parse (alexScanTokens input)

  -- | Perform semantic analysis; on success emit DOT, else print errors.
  case checkAll ast of
    []   -> T.putStr (programToDot ast)
    errs -> mapM_ print errs >> exitFailure

-- | Generate a complete DOT graph for the given program AST.
programToDot :: Program -> Text
programToDot (Program decls) =
     "digraph AST {\n"
  <> "  node [shape=box, fontname=\"Courier\"];\n"
  <> T.concat (zipWith (\i d -> declToDot ("decl" <> T.pack (show i)) d) [0..] decls)
  <> "}\n"

-- | Render a declaration as a DOT subgraph, labeling and connecting its components.
declToDot :: Text -> Decl -> Text
declToDot name = \case
  FunDecl f ps b ->
    let lbl   = "FunDecl\n" <> T.pack f <> "(" <> T.intercalate "," (map T.pack ps) <> ")"
        bodyN = name <> "_body"
    in  node name lbl
     <> node bodyN (exprLabel b)
     <> edge name bodyN
     <> exprToDot bodyN b

-- | Create a human-readable label for an expression node.
exprLabel :: Expr -> Text
exprLabel = \case
  Var x        -> "Var\n"   <> T.pack x
  Lit _        -> "Lit"
  Lambda ps _  -> "Lambda(" <> T.intercalate "," (map T.pack ps) <> ")"
  If{}         -> "If"
  Case{}       -> "Case"
  Let{}        -> "Let"
  App{}        -> "App"
  BinOp op _ _ -> "BinOp\n" <> T.pack (show op)
  UnOp op _    -> "UnOp\n" <> T.pack (show op)
  List{}       -> "List"
  Tuple{}      -> "Tuple"

-- | Recursively render children of an expression node.
exprToDot :: Text -> Expr -> Text
exprToDot prefix = \case
  Var{}    -> ""
  Lit{}    -> ""
  Lambda _ b -> child prefix "body" b

  If c t e -> children prefix ["cond","then","else"] [c,t,e]

  Case s alts ->
       child prefix "scrut" s
    <> T.concat [ altToDot prefix i pat bd
                | (i,(pat,bd)) <- zip [0..] alts ]

  Let ds e ->
       T.concat [ declToDot (prefix <> "_let" <> T.pack (show i)) d
                | (i,d) <- zip [0..] ds ]
    <> child prefix "in" e

  App f x -> children prefix ["fun","arg"] [f,x]

  BinOp _ l r -> children prefix ["l","r"] [l,r]

  UnOp _ x -> child prefix "arg" x

  List xs -> T.concat [ child prefix (T.pack $ "e"++show i) x
                       | (i,x) <- zip [0..] xs ]

  Tuple xs -> T.concat [ child prefix (T.pack $ "e"++show i) x
                        | (i,x) <- zip [0..] xs ]
  where
    -- | Render a single child node and connect it.
    child p role e =
      let n = p <> "_" <> role
      in  node n (exprLabel e)
       <> edge p n
       <> exprToDot n e

    -- | Render multiple children with given roles.
    children p rs es = T.concat $ zipWith (\r e -> child p r e) rs es

-- | Render a case alternative (pattern and body) as DOT.
altToDot :: Text -> Int -> Pattern -> Expr -> Text
altToDot prefix i pat bd =
  let pn = prefix <> "_pat" <> T.pack (show i)
      bn = prefix <> "_bd"  <> T.pack (show i)
  in  node pn (patternLabel pat)
   <> edge prefix pn
   <> node bn "AltBody"
   <> edge pn bn
   <> exprToDot bn bd

-- | Create a label for a pattern node.
patternLabel :: Pattern -> Text
patternLabel = \case
  PWildcard   -> "_"
  PVar x      -> T.pack x
  PLit _      -> "LitPat"
  PList _     -> "ListPat"
  PTuple _    -> "TuplePat"

-- | Define a DOT node with a name and label.
node :: Text -> Text -> Text
node name label =
  "  " <> name <> " [label=\"" <> label <> "\"];\n"

-- | Define a DOT edge between two nodes.
edge :: Text -> Text -> Text
edge from to =
  "  " <> from <> " -> " <> to <> ";\n"
