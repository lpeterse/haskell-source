{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Source.List (toList, fromList)
import Data.Source.Attoparsec
import Data.Attoparsec.ByteString.Char8 (anyChar)

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main  = defaultMain $ testGroup "Data" [ testGroup "Source" [tgAttoparsec]]

tgAttoparsec :: TestTree
tgAttoparsec = testGroup "Attoparsec"
  [ tgAttoparsecParse
  ]

tgAttoparsecParse :: TestTree
tgAttoparsecParse =
  testGroup "parse"
  [ testCase "001 anyChar" $ assertEqual
     "Parsing heads from single input ByteString"
      ( Just ['a','b','c'] )
      ( toList $ parse anyChar $ fromList ["abc"] )

  , testCase "002 anyChar" $ assertEqual
      "Parsing heads from multiple input ByteStrings"
      ( Just ['a','b','c','d','e','f'] )
      ( toList $ parse anyChar $ fromList ["abc", "d", "ef"] )

  , testCase "003 anyChar" $ assertEqual
      "Parsing heads from multiple input ByteStrings with some being empty"
      ( Just ['a','b','c','d','e','f'] )
      ( toList $ parse anyChar $ fromList ["abc", "", "de", "", "", "f"] )
  ]
