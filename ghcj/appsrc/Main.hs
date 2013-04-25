module Main where


import Control.Monad.State (runStateT)
import Control.Monad.Trans (lift)
import MonadUtils
import System.IO (hClose, openTempFile)
import System.IO.Temp (withSystemTempDirectory)

import GHC
import DynFlags
import GhcMonad (unGhc)

import qualified Data.Aeson.Generic as AE

import GHCJ
import Evaluation
import Types

fout :: FlushOut
fout = defaultFlushOut

main :: IO ()
main = do withSystemTempDirectory "iHaskell.shared" $ \tdir -> do
            (tmpFile, hdl) <- openTempFile tdir "iHaskell.vals"
            hClose hdl -- close handle, we don't need it yet.
            session <- initSession tmpFile tdir
            let istate = initialState tmpFile tdir
            _ <- defaultErrorHandler defaultFatalMessager fout $
                 unGhc (runStateT loop istate) session
            return ()
    where loop = do input <- lift $ liftIO getLine
                    result <- evalLine input
                    lift $ liftIO $ print $ AE.encode result
                    loop