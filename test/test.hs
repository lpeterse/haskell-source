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
  [ testCase "001 anyChar on single ByteString" $ assertEqual ""
      ( Just ['a','b','c'] )
      ( toList $ parse anyChar $ fromList ["abc"] )

  , testCase "002 anyChar over multiple ByteStrings" $ assertEqual ""
      ( Just ['a','b','c','d','e','f'] )
      ( toList $ parse anyChar $ fromList ["abc", "d", "ef"] )

  , testCase "003 anyChar over multiple ByteStrings with some being empty" $ assertEqual ""
      ( Just ['a','b','c','d','e','f'] )
      ( toList $ parse anyChar $ fromList ["abc", "", "de", "", "", "f"] )

  , testCase "004 anyChar on empty source" $ assertEqual ""
      ( Just [] )
      ( toList $ parse anyChar $ fromList [] )

  , testCase "005 anyChar on source with empty ByteString" $ assertEqual ""
      ( Just [] )
      ( toList $ parse anyChar $ fromList [""] )

  , testCase "006 tuples of two from even ByteString" $ assertEqual ""
      ( Just [('a','b'),('c','d')] )
      ( toList $ parse (anyChar >>= \a-> anyChar >>= \b-> return (a,b)) $ fromList ["abcd"])

  , testCase "006 tuples of two from uneven ByteString" $ assertEqual ""
      ( Just [('a','b'),('c','d')] )
      ( toList $ parse (anyChar >>= \a-> anyChar >>= \b-> return (a,b)) $ fromList ["abcde"])
  ]
