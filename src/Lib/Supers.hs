{-# LANGUAGE OverloadedStrings #-}

module Lib.Supers (runHscCached) where

import           Control.Concurrent.MVar
import           Control.Exception       (bracket_)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict as M
import           Data.ByteString.Lazy    (ByteString)
import qualified Data.ByteString.Lazy    as BL
import           Crypto.Hash.SHA1        (hashlazy)
import           System.IO.Temp          (withSystemTempDirectory)
import           System.Process          (readProcess)
import           System.IO.Unsafe        (unsafePerformIO)   -- <<< AQUI

-- cache SHA-1 → resultado compilado
{-# NOINLINE ramCache #-}
ramCache :: MVar (Map String (IO String))
ramCache = unsafePerformIO (newMVar M.empty)

----------------------------------------------------------------------
-- | Compila/faz run de um snippet Haskell só uma vez por hash.
runHscCached :: ByteString -> IO String
runHscCached code = do
  let h = show (hashlazy code)
  tbl <- readMVar ramCache
  case M.lookup h tbl of
    Just act -> act                   -- já em cache
    Nothing  -> do
      let compileOnce = withSystemTempDirectory ("super-" <> h) $ \dir -> do
            let src = dir <> "/Tmp.hs"
            BL.writeFile src code
            -- compila e roda via runghc  (exemplo simples)
            readProcess "runghc" [src] ""
      -- coloca *antes* de rodar para evitar corrida
      mv <- newEmptyMVar
      let thunk = takeMVar mv
      modifyMVar_ ramCache (pure . M.insert h thunk)
      result <- compileOnce
      putMVar mv result
      pure result
