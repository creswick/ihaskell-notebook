{-# LANGUAGE OverloadedStrings #-}
module JsonIO where

import Test.HUnit      ( (@=?), Assertion )
import Test.Framework.Providers.HUnit
import Test.Framework ( testGroup, Test )

import Data.Aeson.Generic

import GHCJ

tests :: [Test]
tests = [ testGroup "JSON Input round-trip"
          [ testCase "Basic Data" $ roundTripInput (Input 1 "Test")
          , testCase "Empty code" $ roundTripInput (Input 1 "")
          ]
        , testGroup "JSON Input parsing"
          [ testCase "Basic Data" $ parsesInput "{\"inputCellNo\": 1, \"inputSource\": \"Test\"}" (Just $ Input 1 "Test")
          , testCase "Empty code" $ parsesInput "{\"inputCellNo\": 1, \"inputSource\": \"\"}" (Just $ Input 1 "")
          , testCase "Parse Fails" $ parsesInput "{}" (Nothing :: Maybe Input)
          ]
        ]

-- | Check for round-triping via JSON for input:
roundTripInput :: Input -> Assertion
roundTripInput input = (Just input) @=? (decode $ encode input)

-- parsesInput :: ByteString -> Maybe Input -> Assertion
parsesInput str oracle = decode str @=? oracle