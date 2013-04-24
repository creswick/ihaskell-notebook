module GHCJ
where

import Control.Monad.State

import Text.JSON

-- | The JSON string for the output data. (An Either Error Success value)
data_key_str :: String
data_key_str = "result"

-- | The name of the cell id string in json input/output
id_key_str :: String
id_key_str = "id"

-- | The name of the source key in the json input/output
src_key_str :: String
src_key_str = "source"

data Input =
    Input { inputCellNo :: Int
          , inputSource :: String
          } deriving (Show)

data (JSON j, JSON i) => Output j i =
    Output { outputCellNo :: Int
           , outputData   :: Either j i
           -- ^ Left to indicate an error, Right indicates success.
           } deriving (Show)

instance (JSON i, JSON j) => JSON (Output j i) where
    readJSON (JSObject jso) =
        let table = fromJSObject jso
        in case (lookup id_key_str table, lookup data_key_str table) of
             (Just cn, Just outData) -> do
               cId <- readJSON cn
               dat <- readJSON outData
               return Output { outputCellNo = cId
                             , outputData   = dat }
             _                   -> Error $ show jso
    readJSON input            = Error $ show input

    showJSON (Output cellNo dat) = JSObject $ toJSObject [ (id_key_str, showJSON cellNo)
                                                         , (data_key_str, showJSON dat)]

instance JSON Input where
    -- readJSON  :: JSValue -> Result Input
    readJSON (JSObject jso) =
        let table = fromJSObject jso
        in case (lookup id_key_str table, lookup src_key_str table) of
             (Just cn, Just src) -> do
               cId <- readJSON cn
               code <- readJSON src
               return Input { inputCellNo = cId
                            , inputSource = code }
             _                   -> Error $ show jso
    readJSON input            = Error $ show input

    -- showJSON :: Input -> JSValue
    showJSON (Input cellNo source) = JSObject $ toJSObject [ (id_key_str, showJSON cellNo)
                                                           , (src_key_str, showJSON source)]

-- | Data type to hold the GHC API state, for now, it's mostly a placeholder.
data EvalState = EState { modules :: [Input] -- ^ A list of the successfull builds
                        } deriving (Show)

initialState :: EvalState
initialState = EState { modules = [] }

evaluate :: (JSON i, JSON j) => Input -> State EvalState (Output j i)
evaluate = undefined