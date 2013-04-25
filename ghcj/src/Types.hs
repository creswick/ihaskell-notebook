{-# LANGUAGE DeriveDataTypeable #-}
module Types where

import Data.Aeson.Generic() -- for instances
import Data.Typeable (Typeable)
import Data.Data (Data)
import GHC.IO.Handle (Handle)

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
data EvalState = EState { estateTmpFile :: (FilePath, Handle)
                        -- ^ The temporary file used to share 'it' values.
                        } deriving (Show)

initialState :: FilePath -> Handle -> EvalState
initialState tmpFile hdl = EState { estateTmpFile = (tmpFile, hdl)
                                  }
