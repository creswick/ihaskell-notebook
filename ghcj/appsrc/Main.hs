module Main where

import Control.Monad.State (runState)
import System.Environment (getArgs)

import qualified Data.ByteString as BS

import GHCJ

main :: IO ()
main = do input <- getLine
          let result :: (Output, EvalState)
              result = runState (evalLine input) initialState
          print result