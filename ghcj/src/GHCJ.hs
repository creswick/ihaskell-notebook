module GHCJ
where

import Control.Monad.State
import GHC

import qualified Data.Aeson.Generic as AE
import qualified Data.ByteString.Lazy as BL (fromChunks, ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8

import Types
import Evaluation

evalLine :: String -> StateT EvalState Ghc Output
evalLine encLine = case B64.decode $ C8.pack encLine of
                     Left err -> return $ ParseError ("B64 Decoding Failed: "++err)
                     Right json -> evalJsonLine (BL.fromChunks [json])

evalJsonLine :: BL.ByteString -> StateT EvalState Ghc Output
evalJsonLine jsonLine = case AE.decode jsonLine of
                          Nothing    -> return $ ParseError 
                                           ("JSON Decode falied." ++ (show jsonLine))
                          Just input -> evaluate input
                                                           

evaluate :: Input -> StateT EvalState Ghc Output
evaluate (Input cId stmt) = evalStmt stmt