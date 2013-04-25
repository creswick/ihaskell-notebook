module Main where


import Control.Monad.State (runStateT)
import Control.Monad.Trans (lift)
import MonadUtils

import GHC
import DynFlags
import GhcMonad (unGhc)

import GHCJ
import Evaluation
import Types

fout = defaultFlushOut

main :: IO ()
main = do session <- initSession
          _ <- defaultErrorHandler defaultFatalMessager fout $ unGhc (runStateT loop initialState) session
          return ()
    where loop = do input <- lift $ liftIO getLine
                    -- catch exceptions:
                    result <- evalLine input
                    lift $ liftIO $ print result
                    loop
          -- handler :: SomeException -> IO Output
          -- handler e = do print e
          --                return $ CompileError (show e)