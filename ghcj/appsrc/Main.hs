module Main where

import Data.Char (isSpace)
import Control.Monad.State (runStateT)
import Control.Monad.Trans (lift)
import Data.Data (Data)
import MonadUtils
import System.IO (hClose, openTempFile)
import System.IO.Temp (withSystemTempDirectory)

import GHC
import Exception
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

fmessager :: FatalMessager
fmessager err = print $ toB64Json $ CompileError err

main :: IO ()
main = do withSystemTempDirectory "iHaskell.shared" $ \tdir -> do
            (tmpFile, hdl) <- openTempFile tdir "iHaskell.vals"
            hClose hdl -- close handle, we don't need it yet.
            session <- initSession tmpFile tdir
            let istate = initialState tmpFile tdir
            _ <- defaultErrorHandler fmessager fout $
                 unGhc (runStateT loop istate) session
            return ()
         --loop :: StateT EvalState Ghc b
    where loop = do input <- lift $ liftIO getLine
                    case trim input of
                      [] -> loop
                      _  -> do result <- evalLine input
                               lift $ liftIO $ print $ toB64Json result
                               loop

toB64Json :: Data.Data.Data a => a -> B.ByteString
toB64Json r = B64.encode $ toStrict $ AE.encode r

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- -- | Install some default exception handlers and run the inner computation.
-- -- Unless you want to handle exceptions yourself, you should wrap this around
-- -- the top level of your program.  The default handlers output the error
-- -- message(s) to stderr and exit cleanly.
-- customErrorHandler :: (ExceptionMonad m, MonadIO m)
--                     => FatalMessager -> FlushOut -> m a -> m a
-- customErrorHandler fm (FlushOut flushOut) inner =
--   -- top-level exception handler: any unrecognised exception is a compiler bug.
--   ghandle (\exception -> liftIO $ do
--            flushOut
--            case fromException exception of
--                 -- an IO exception probably isn't our fault, so don't panic
--                 Just (ioe :: IOException) ->
--                   fatalErrorMsg'' fm (show ioe)
--                 _ -> case fromException exception of
--                      Just UserInterrupt -> exitWith (ExitFailure 1)
--                      Just StackOverflow ->
--                          fatalErrorMsg'' fm "stack overflow: use +RTS -K<size> to increase it"
--                      _ -> case fromException exception of
--                           Just (ex :: ExitCode) -> throw ex
--                           _ ->
--                               fatalErrorMsg'' fm
--                                   (show (Panic (show exception)))
--            exitWith (ExitFailure 1)
--          ) $

--   -- error messages propagated as exceptions
--   handleGhcException
--             (\ge -> liftIO $ do
--                 flushOut
--                 case ge of
--                      PhaseFailed _ code -> exitWith code
--                      Signal _ -> exitWith (ExitFailure 1)
--                      _ -> do fatalErrorMsg'' fm (show ge)
--                              exitWith (ExitFailure 1)
--             ) $
--   inner