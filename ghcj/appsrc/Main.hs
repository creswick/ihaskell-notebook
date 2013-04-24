module Main where

import Control.Monad.State (runState)

import GHCJ

main :: IO ()
main = do input <- getLine
          let result :: (Output, EvalState)
              result = runState (evalLine input) initialState
          print $ fst result