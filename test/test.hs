{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Source.List (toList, fromList)
import Data.Source.Attoparsec
import Data.Attoparsec.ByteString.Char8 (anyChar)

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main  = defaultMain $ testGroup "Data.Source" [tAttoparsec]

tAttoparsec :: TestTree
tAttoparsec = testGroup "Data.Source.Attoparsec"
  [ testCase "anyChar (001)" $ assertEqual
     "Parsing heads from single input ByteString"
      ( Just ['a','b','c'] )
      ( toList $ parse anyChar $ fromList ["abc"] )

  , testCase "anyChar (002)" $ assertEqual
      "Parsing heads from multiple input ByteStrings"
      ( Just ['a','b','c','d','e','f'] )
      ( toList $ parse anyChar $ fromList ["abc", "d", "ef"] )

  , testCase "anyChar (003)" $ assertEqual
      "Parsing heads from multiple input ByteStrings with some being empty"
      ( Just ['a','b','c','d','e','f'] )
      ( toList $ parse anyChar $ fromList ["abc", "", "de", "", "", "f"] )
  ]
