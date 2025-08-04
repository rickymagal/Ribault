{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Lib.Supers
  ( runHscCached
  , super_isNil
  , super_cons
  , super_headList
  , super_tailList
  ) where

import           Control.Concurrent.MVar
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as M
import           Data.ByteString.Lazy    (ByteString)
import qualified Data.ByteString.Lazy    as BL
import           Crypto.Hash.SHA1        (hashlazy)
import           System.IO.Temp          (withSystemTempDirectory)
import           System.Process          (readProcess)
import           System.IO.Unsafe        (unsafePerformIO)

import           Foreign.StablePtr       (StablePtr, newStablePtr, deRefStablePtr)
import           Foreign.C.Types         (CInt)

-------------------------------------------------------------------------------
-- | Cache SHA-1 → ação compilada
-------------------------------------------------------------------------------
{-# NOINLINE ramCache #-}
ramCache :: MVar (Map String (IO String))
ramCache = unsafePerformIO (newMVar M.empty)

-------------------------------------------------------------------------------
-- | Compila/executa snippet Haskell apenas uma vez por hash
-------------------------------------------------------------------------------
runHscCached :: ByteString -> IO String
runHscCached code = do
  let h = show (hashlazy code)
  tbl <- readMVar ramCache
  case M.lookup h tbl of
    Just act -> act
    Nothing  -> do
      mv <- newEmptyMVar
      modifyMVar_ ramCache (pure . M.insert h (takeMVar mv))
      let compileOnce = withSystemTempDirectory ("super-" <> h) $ \dir -> do
            let src = dir <> "/Tmp.hs"
            BL.writeFile src code
            readProcess "runghc" [src] ""
      result <- compileOnce
      putMVar mv result
      pure result

-------------------------------------------------------------------------------
-- | Tipo de valor para super-instruções
-------------------------------------------------------------------------------
data Value
  = VInt  CInt
  | VBool CInt           -- 1 = True, 0 = False
  | VList [StablePtr Value]

-------------------------------------------------------------------------------
-- | Cria um novo Value e retorna um ponteiro estável (StablePtr)
-------------------------------------------------------------------------------
newValue :: Value -> IO (StablePtr Value)
newValue = newStablePtr

-- | Extrai lista de ponteiros de Value
-------------------------------------------------------------------------------
peekList :: StablePtr Value -> IO [StablePtr Value]
peekList sp = do
  VList xs <- deRefStablePtr sp
  pure xs

-- | Empacota lista de ponteiros em Value
-------------------------------------------------------------------------------
pokeList :: [StablePtr Value] -> IO (StablePtr Value)
pokeList xs = newValue (VList xs)

-------------------------------------------------------------------------------
-- | super_isNil: testa se lista vazia (1 ou 0)
-------------------------------------------------------------------------------
foreign export ccall super_isNil :: StablePtr Value -> IO (StablePtr Value)
super_isNil pList = do
  xs <- peekList pList
  newValue (VBool (if null xs then 1 else 0))

-------------------------------------------------------------------------------
-- | super_cons: constrói lista (head : tail)
-------------------------------------------------------------------------------
foreign export ccall super_cons :: StablePtr Value -> StablePtr Value -> IO (StablePtr Value)
super_cons pHead pTail = do
  tailList <- peekList pTail
  newValue (VList (pHead : tailList))

-------------------------------------------------------------------------------
-- | super_headList: retorna cabeça de lista não-vazia
-------------------------------------------------------------------------------
foreign export ccall super_headList :: StablePtr Value -> IO (StablePtr Value)
super_headList pList = do
  xs <- peekList pList
  case xs of
    (h:_) -> pure h
    []    -> error "super_headList: lista vazia"

-------------------------------------------------------------------------------
-- | super_tailList: retorna cauda de lista não-vazia
-------------------------------------------------------------------------------
foreign export ccall super_tailList :: StablePtr Value -> IO (StablePtr Value)
super_tailList pList = do
  xs <- peekList pList
  case xs of
    (_:t) -> pokeList t
    []    -> error "super_tailList: lista vazia"