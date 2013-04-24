{-# LANGUAGE DeriveDataTypeable #-}
module GHCJ
where

import Control.Monad.State

import qualified Data.ByteString.Lazy as BL (fromChunks)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8

-- import Text.JSON

import Data.Aeson.Generic
import qualified Data.Aeson.Generic as AE

import Data.Typeable
import Data.Data

data Input = Input { inputCellNo :: Int
                   , inputSource :: String
                   } deriving (Eq, Show, Data, Typeable)

data Output = ParseError String
            | CompileError String
            | CompileWarning String
            | Output { outputCellNo :: Int
                     , outputData   :: String
                     }
              deriving (Eq, Show, Data, Typeable)

-- | Data type to hold the GHC API state, for now, it's mostly a placeholder.
data EvalState = EState { modules :: [Input] -- ^ A list of the successfull builds
                        } deriving (Show)

initialState :: EvalState
initialState = EState { modules = [] }

evalLine :: String -> State EvalState Output
evalLine encLine = case B64.decode $ C8.pack encLine of
                     Left err -> return $ ParseError ("B64 Decoding Failed: "++err)
                     Right json -> evalJsonLine (BL.fromChunks [json])

-- evalJsonLine :: ByteString -> State EvalState Output
evalJsonLine jsonLine = case AE.decode jsonLine of
                          Nothing    -> return $ ParseError 
                                           ("JSON Decode falied." ++ (show jsonLine))
                          Just input -> evaluate input
                                                           

evaluate :: Input -> State EvalState Output
evaluate (Input cId code) = return Output { outputCellNo = cId
                                          , outputData = "Done! (but, didn't do anything)"
                                          }

-- evalLine encodedLn = return $ let eJson = B64.decode $ C8.pack encodedLn
--                               in case eJson of
--                                 Left   err -> ParseError 
--                                 Right json -> 
--                                          let decoded :: Result Input
--                                              decoded = decode $ C8.unpack json
--                                          in case decoded of
--                                               Error str           ->
--                                                   ParseError ("JSON Decode falied: "++str)
--                                               Ok (Input cId code) ->
--                                                   case decode code of
--                                                     Error eStr -> ParseError $ "Code: "++code
--                                                     Ok    cStr -> Output cId cStr

-- process :: Input -> State EvalState (Output String)
-- process (Input cId code) = return $ Output cId "done! (but I didn't do anything)" 