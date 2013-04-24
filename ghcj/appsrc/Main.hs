module Main where

import Control.Monad.State (runState)
import System.Environment (getArgs)
import Text.JSON (decode, encode, Result(..))

import GHCJ

main :: IO ()
main = do args <- getArgs
          let parsed :: Result Input
              parsed = decode (args!!0)
          case parsed of
            Ok  input -> print $ encode input --let (output, st) = runState (evaluate input) initialState
            Error err -> print err