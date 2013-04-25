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

main :: IO ()
main = do session <- initSession
          defaultErrorHandler defaultFatalMessager defaultFlushOut $ unGhc (runStateT loop initialState) session
          return ()
    where loop = do input <- lift $ liftIO getLine
                    result <- evalLine input
                    lift $ liftIO $ print result
                    loop