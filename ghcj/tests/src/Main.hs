module Main where

import Test.Framework ( defaultMain )

import qualified JsonIO as JsonIO

main :: IO ()
main = defaultMain $ concat [ JsonIO.tests ]