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
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL (toChunks, ByteString)
import qualified Data.ByteString.Base64 as B64


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
                    lift $ liftIO $ print $ B64.encode $ toStrict $ AE.encode result
                    loop

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks