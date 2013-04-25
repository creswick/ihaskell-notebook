module Main where


import Control.Monad.State (runStateT)
import Control.Monad.Trans (lift)
import MonadUtils
import System.IO.Temp (withSystemTempFile)

import GHC
import DynFlags
import GhcMonad (unGhc)

import GHCJ
import Evaluation
import Types

fout :: FlushOut
fout = defaultFlushOut

main :: IO ()
main = do withSystemTempFile "iHaskell.shared" $ \tfile hdl -> do
            session <- initSession tfile
            let istate = initialState tfile hdl
            _ <- defaultErrorHandler defaultFatalMessager fout $
                 unGhc (runStateT loop istate) session
            return ()
    where loop = do input <- lift $ liftIO getLine
                    result <- evalLine input
                    lift $ liftIO $ print result
                    loop